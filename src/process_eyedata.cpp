#include <algorithm>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame findRealBlinks(NumericVector starttimes,NumericVector endtimes, NumericVector types) {
  // given saccade and blink events, code the saccades immediately before and after blinks as blinks
  int n = types.size();
  NumericVector out(types.size()); //binary vector indicating whether it should be counted as a blink

  for(int i=1; i<n;i++)
  {
    if (types[i]==4) //blinks are blinks
      out[i]=1;

    if (types[i]==4 & types[i-1]==6 & endtimes[i-1]>starttimes[i]) //previous saccades overlapping with blinks are blinks
    {
      out[i-1]=1;
    }
    else if (types[i]==6 & types[i-1]==4 & endtimes[i-1]>starttimes[i] ) //saccades following blinks that are overlapping are also blinks
      out[i]=1;
  }

  return DataFrame::create(_["sttime"]= starttimes, _["entime"]= endtimes,_["type"]=types,_["blink"]=out);
}

// [[Rcpp::export]]
arma::mat events2samples(NumericVector starttimes, NumericVector endtimes, NumericVector vals)
{
  // take event data with starting and ending times and convert to long format (i.e., sample-by-sample)

  // convert to Armadillo vectors
  arma::vec st=arma::vec(starttimes);
  arma::vec en=arma::vec(endtimes);
  arma::vec val=arma::vec(vals);

  // adjust the raw time to relative time from first sample
  double mintime = min(st);
  arma::vec st_adj = st - mintime;
  arma::vec en_adj = en - mintime;

  //make a blank vector that's as big as the maximum number of samples (plus some padding)
  arma::mat newsamp = arma::mat(max(en_adj)+100,4,arma::fill::none);

  //fill first column with the sample number (adjusted back to raw time)
  arma::vec sampnum = arma::linspace(0, max(en_adj)+99,max(en_adj)+100);
  newsamp(arma::span::all,0) = sampnum + mintime;

  unsigned int idx1,idx2;
  int fillsize;
  arma::mat val_fill,st_fill,en_fill;

  //for each event
  // starttimes.size()-1
  for(int i=0; i<starttimes.size()-1; i++)
  {

    //get the starting and ending sample
    idx1=st_adj(i);
    idx2=en_adj(i);


    //create a vector that is big enough for start to end sample
    fillsize = idx2-idx1+1;

    // Rprintf("idx1=%i, idx2=%i\n",idx1,idx2);


    //replicate the start time that many times
    st_fill =  arma::mat(fillsize,1,arma::fill::none);
    st_fill.fill(idx1+mintime); //bring back into our raw time

    //fill in our output matrix (column 2)
    newsamp(arma::span(idx1,idx2),1) = st_fill;

    //rinse and repeat for end times
    en_fill =  arma::mat(fillsize,1,arma::fill::none);
    en_fill.fill(idx2+mintime);

    newsamp(arma::span(idx1,idx2),2) = en_fill;

    //and for the values
    val_fill =  arma::mat(fillsize,1,arma::fill::none);
    val_fill.fill(val(i));

    newsamp(arma::span(idx1,idx2),3) = val_fill;
  }

  // some timepoints will be unaccounted for, we don't care
  // about them, throw those rows out
  arma::uvec noblanks = find(newsamp.col(1) > 0);
  newsamp = newsamp.rows( noblanks );


  return(newsamp);

}


arma::mat epoch_index(NumericVector timelock, int startepoch, int endepoch )
{

  //given a vector of start times and epoch start and end, generate a matrix of indices
  //for selecting out epochs of data
  unsigned int idx1,idx2;
  int fillsize;

  arma::mat epochs = arma::mat(timelock.size(),endepoch-startepoch);
  arma::vec filler = arma::vec(endepoch-startepoch);

  //for each event
  // starttimes.size()-1
  for(int i=0; i<timelock.size(); i++)
  {

    //get the starting and ending sample
    idx1=timelock(i) + startepoch;
    idx2=timelock(i) + endepoch;

    //create a vector that is big enough for start to end sample
    fillsize = idx2-idx1;
    filler = arma::linspace<arma::vec>(idx1,idx2,fillsize);

    epochs.row(i) = filler.t();

  }

  return(epochs);
}

// [[Rcpp::export]]
arma::mat get_epochs(NumericVector eventtimes, NumericVector samptimes, NumericVector sampvals, int startepoch, int endepoch)
{
  //given event times and sample data, get epochs of the sample data relative to those events

  // convert to Armadillo vectors
  arma::vec stimes=arma::vec(samptimes);
  arma::uvec times = arma::conv_to<arma::uvec>::from(stimes);
  arma::vec vals=arma::vec(sampvals);

  //get indices for each epoch, convert to integer matrix
  arma::mat idx = epoch_index(eventtimes,startepoch,endepoch);
  arma::imat epoch_idx = arma::conv_to<arma::imat>::from(idx);

  int mintime = min(times);
  times -= mintime; //so sample times are 0:numsamples
  epoch_idx -= mintime; //likewise for the epoch indices

 // we need a full vector for samples so we can index proper times
 // the original will have gaps for when the tracker was paused.
  arma::vec fullval = arma::vec(max(times)+1,1,arma::fill::zeros);
  fullval.elem(times) = vals;

  // covering cases where epochs extend before beginning or after the end of recording
  arma::uvec tmp, tmp2;
  tmp = find(epoch_idx < 0); //if the epoch extends before the start of recording
  epoch_idx.elem(tmp) *= 0; //replicate the first index

  tmp2 = find(epoch_idx > max(times)); //if it extends beyond the end of recording
  arma::ivec endvec = arma::ivec(size(tmp2));
  endvec.fill(max(times));
  epoch_idx.elem(tmp2) = endvec; //replicate the last index


  //start with a blank matrix that is events x epoch_size
  arma::mat result = arma::mat(eventtimes.size(),endepoch - startepoch,arma::fill::none);
  //our temporary variables as we loop
  arma::uvec indices;
  arma::vec temp;

  for (int i=0; i<epoch_idx.n_rows; i++)
  {
    //loop through each epoch, grab indices from that row
    indices = arma::conv_to<arma::uvec>::from(epoch_idx.row(i));
    temp= fullval.elem(indices); //grab the corresponding sample data (this will be a column vector)
    result.row(i) = temp.t(); //fill in result
  }

  return(result);
}
