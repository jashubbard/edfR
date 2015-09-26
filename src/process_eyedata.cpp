#include <Rcpp.h>
#include <algorithm>

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
List findRealBlinks(NumericVector starttimes,NumericVector endtimes, NumericVector types) {


  int n = types.size();
  NumericVector out(types.size());

  for(int i=1; i<n;i++)
  {
    if (types[i]==4)
      out[i]=1;

    if (types[i]==4 & types[i-1]==6 & endtimes[i-1]>starttimes[i])
    {
      out[i-1]=1;
    }
    else if (types[i]==6 & types[i-1]==4 & endtimes[i-1]>starttimes[i] )
      out[i]=1;
  }


  double mintime = min(starttimes);
  NumericVector start_adj = starttimes - mintime;
  NumericVector end_adj = endtimes - mintime;


    std::vector<int> myvector(int(end_adj[end_adj.size()-1]),0);

    for(int i=1; i<n;i++)
    {

      if(out[i]==1)
          std::fill(myvector.begin()+start_adj[i],myvector.begin()+end_adj[i],1);
    }

    int sum_of_elems =std::accumulate(myvector.begin(),myvector.end(),0);//#include <numeric>

    std::vector<int>::iterator iter = myvector.begin();
    std::vector<int> newtimes(myvector.size());
    std::iota(newtimes.begin(), newtimes.end(), mintime);


  return List::create(starttimes,start_adj,endtimes,end_adj,sum_of_elems,newtimes,myvector);
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

