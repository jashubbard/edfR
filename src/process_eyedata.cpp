
#include <algorithm>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector findInterval2(NumericVector x, NumericVector breaks) {

  // example function found online, might be useful later
  IntegerVector out(x.size());

  NumericVector::iterator it, pos;
  IntegerVector::iterator out_it;

  for(it = x.begin(), out_it = out.begin(); it != x.end();
  ++it, ++out_it) {
    pos = std::upper_bound(breaks.begin(), breaks.end(), *it);
    *out_it = std::distance(breaks.begin(), pos);
  }

  return out;
}

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
arma::mat events2samples(NumericVector starttimes, NumericVector endtimes,NumericVector vals)
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

  //make a blank vector that's as big as the maximum number of samples
  arma::mat newsamp = arma::mat(max(en_adj),4,arma::fill::none);

  //fill first column with the sample number (adjusted back to raw time)
  arma::vec sampnum = arma::linspace(0, max(en_adj)-1,max(en_adj));
  newsamp(arma::span::all,0) = sampnum + mintime;

  unsigned int idx1,idx2;
  int fillsize;
  arma::mat val_fill,st_fill,en_fill;

  //for each event
  for(int i=0; i<starttimes.size()-1; i++)
  {

    //get the starting and ending sample
    idx1=st_adj(i);
    idx2=en_adj(i);

    //create a vector that is big enough for start to end sample
    fillsize = idx2-idx1+1;
    // Rprintf("fillsize=%i\n",fillsize);

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


