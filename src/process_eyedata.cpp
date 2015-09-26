
#include <algorithm>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// This is a simple function using Rcpp that creates an R list
// containing a character vector and a numeric vector.
//
// Learn more about how to use Rcpp at:
//
// http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//
// and browse examples of code using Rcpp at:

//   http://gallery.rcpp.org/
//


List rcpp_hello() {
  CharacterVector x = CharacterVector::create("foo", "bar");
  NumericVector y   = NumericVector::create(0.0, 1.0);
  List z            = List::create(x, y);
  return z;
}


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


//   double mintime = min(starttimes);
//   NumericVector start_adj = starttimes - mintime;
//   NumericVector end_adj = endtimes - mintime;
//
//   // here's how we print out a vector via C++
//   // Rcpp::Rcout << "end_adj is " << std::endl << end_adj << std::endl;
//
//
//
//   std::vector<int> myvector(int(end_adj[end_adj.size()-1]),0);
//
//   for(int i=1; i<n;i++)
//   {
//
//     if(out[i]==1)
//       std::fill(myvector.begin()+start_adj[i],myvector.begin()+end_adj[i],1);
//   }
//
//   int sum_of_elems =std::accumulate(myvector.begin(),myvector.end(),0);//#include <numeric>
//
//   std::vector<int>::iterator iter = myvector.begin();
//   std::vector<int> newtimes(myvector.size());
//   std::iota(newtimes.begin(), newtimes.end(), mintime);
//

  return DataFrame::create(_["sttime"]= starttimes, _["entime"]= endtimes,_["type"]=types,_["blink"]=out);
}

// [[Rcpp::export]]
arma::mat events2samples(NumericVector starttimes, NumericVector endtimes,NumericVector vals)
{

  arma::vec st=arma::vec(starttimes);
  arma::vec en=arma::vec(endtimes);
  arma::vec val=arma::vec(vals);

  double mintime = min(st);

  arma::vec st_adj = st - mintime;
  arma::vec en_adj = en - mintime;

  //make a blank vector that's as big as the maximum number of samples
  arma::mat newsamp = arma::mat(max(en_adj),4,arma::fill::none);

  //fill first column with the sample number (adjusted to raw time)
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

    //create a vector that is big enough for start:end sample
    fillsize = idx2-idx1+1;

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


// [[Rcpp::export]]
std::vector<int> expandrange(NumericVector starttimes,NumericVector endtimes, NumericVector evt)
{

  int n = evt.size();
  double mintime = min(starttimes);
  NumericVector start_adj = starttimes - mintime;
  NumericVector end_adj = endtimes - mintime;



  std::vector<int> myvector(int(end_adj[end_adj.size()-1]),0);

  for(int i=1; i<n;i++)
  {

    if(evt[i]==1)
      std::fill(myvector.begin()+start_adj[i],myvector.begin()+end_adj[i],1);
  }

  int sum_of_elems =std::accumulate(myvector.begin(),myvector.end(),0);//#include <numeric>

  std::vector<int>::iterator iter = myvector.begin();
  std::vector<int> newtimes(myvector.size());
  std::iota(newtimes.begin(), newtimes.end(), mintime);

  return(newtimes);

}

// [[Rcpp::export]]
arma::mat matrixSubset(arma::mat M, int val) {
  // logical conditionL where is transpose larger?
  arma::umat a = M > val;
  arma::mat  N = arma::conv_to<arma::mat>::from(a);
  return N;
}

// [[Rcpp::export]]
arma::vec matrixFind(arma::mat M, int val) {
  arma::vec v = M.elem( arma::find( M >= val ) );
  return v;
}

// [[Rcpp::export]]
arma::vec vectorFind(arma::vec M, int val) {
  arma::uvec v = arma::find( M >= val );

  // change elements of M greater than 0.5 to 1
  M.elem( find(M > 0.5) ).ones();

  return M;
}

