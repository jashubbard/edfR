// this code is heavily based on code from Titus von der Malsburg: http://pages.ucsd.edu/~tvondermalsburg
#include <R.h>
#include <Rdefines.h>
#include "edf.h"
#include "edf_api.hpp"

extern "C" {

#define EDFDEBUG 1


SEXP get_preamble(SEXP filename)
{
	int check(0);
	EDFFILE* ef = edf_open_file( as_string(filename), 0, 0, 0, &check);
	if(ef)
	{
		int text_length = edf_get_preamble_text_length(ef);
		char* preamble = new char[(sizeof(char)*text_length+1)];
		edf_get_preamble_text(ef, preamble, text_length);
		SEXP ans = mkString(preamble);
		delete[] preamble;
		edf_close_file(ef);
		return(ans);
	}
	return(R_NilValue);
}


SEXP get_samples(SEXP filename, SEXP fields)
{
	int check(0);

	EDFFILE* ef = edf_open_file( as_string(filename), 0, 0, 1, &check);

	if(ef)
	{
		int noelem = edf_get_element_count(ef);

#ifdef EDFDEBUG
    Rprintf("Reading Samples from %s (%i elements) ...", as_string(filename),noelem );
#endif

		FSAMPLE* data = new FSAMPLE[noelem];
		int i(0);
		for(int type = edf_get_next_data(ef); type != NO_PENDING_ITEMS; type = edf_get_next_data(ef)){
			if (type==SAMPLE_TYPE){
				data[i++] = edf_get_float_data(ef)->fs;
			// Rprintf("Reading sample %i\n", i);
			}
		}

#ifdef EDFDEBUG
		Rprintf("Success! (%i samples)\n", noelem);
#endif

		SEXP ans;
		int nx(i), ny(LENGTH(fields));
		PROTECT(ans = allocMatrix(REALSXP, nx, ny));
		double* rans = REAL(ans);
		fill_samples(fields,rans,i,data);

		UNPROTECT(1);
		delete[] data;
		edf_close_file(ef);
		return(ans);
	}
	return(R_NilValue);
}


SEXP get_recordings(SEXP filename, SEXP fields)
{
	int check(0);
#ifdef EDFDEBUG
	Rprintf("Reading Recordings from %s ...", as_string(filename) );
#endif
	EDFFILE* ef = edf_open_file( as_string(filename), 0, 1, 0, &check);
	if(ef)
	{
		int noelem = edf_get_element_count(ef);
#ifdef EDFDEBUG
		Rprintf("Success! (%i recordings)\n", noelem);
#endif

		RECORDINGS* data = new RECORDINGS[noelem];
		int i(0);
		for(int type = edf_get_next_data(ef); type != NO_PENDING_ITEMS; type = edf_get_next_data(ef))
			if (type==RECORDING_INFO)
				data[i++] = edf_get_float_data(ef)->rec;
		SEXP ans;
		int nx(i), ny(LENGTH(fields));
		PROTECT(ans = allocMatrix(REALSXP, nx, ny));
		double* rans = REAL(ans);
		fill_recordings(fields,rans,i,data);

		UNPROTECT(1);
		delete[] data;
		edf_close_file(ef);
		return(ans);
	}
	return(R_NilValue);
}


SEXP get_events(SEXP filename, SEXP typlist, SEXP fields)
{
	int check(0);
#ifdef EDFDEBUG
	Rprintf("Reading Events from %s ...", as_string(filename) );
#endif

	EDFFILE* ef = edf_open_file( as_string(filename), 0, 1, 0, &check);
	if(ef)
	{
		int noelem = edf_get_element_count(ef);
#ifdef EDFDEBUG
		Rprintf("Success! (%i events)\n", noelem);
#endif

		FEVENT* data = new FEVENT[noelem];

		int len = LENGTH(typlist);
		int* typelist = as_typelist(typlist,len);

		int i(0);
		for(int type = edf_get_next_data(ef); type != NO_PENDING_ITEMS; type = edf_get_next_data(ef))
			if (is_intmember(type,typelist,len) != -1)
				data[i++] = edf_get_float_data(ef)->fe;


		SEXP ans;
		int nx(i), ny(LENGTH(fields));
		PROTECT(ans = allocMatrix(REALSXP, nx, ny));
		double* rans = REAL(ans);
		fill_events(fields,rans,i,data);

		UNPROTECT(1);
		delete[] data;
		delete[] typelist;
		edf_close_file(ef);
		return(ans);
	}
	return(R_NilValue);
}

SEXP get_messages(SEXP filename)
{
	int check(0);
#ifdef EDFDEBUG
	Rprintf("Reading Messages from %s ...", as_string(filename) );
#endif
	EDFFILE* ef = edf_open_file( as_string(filename), 0, 1, 0, &check);
	if(ef)
	{
		int noelem = edf_get_element_count(ef);
#ifdef EDFDEBUG
		Rprintf("Success! (%i messages)\n", noelem);
#endif

		SEXP ans;
		int nx(noelem), ny(2);
		PROTECT(ans = allocMatrix(VECSXP, nx, ny));

		ALLF_DATA* fd = NULL;
		int i(0);
		for(int type = edf_get_next_data(ef); type != NO_PENDING_ITEMS; type = edf_get_next_data(ef))
		{
			if (type==MESSAGEEVENT)
			{
				fd = edf_get_float_data(ef);
				SET_VECTOR_ELT(ans,i+noelem*0,mkReal(fd->fe.sttime));
				SET_VECTOR_ELT(ans,i+noelem*1,mkString(&(fd->fe.message->c)));
			}
			else
			{
				SET_VECTOR_ELT(ans,i+noelem*0,mkReal(-1));
				SET_VECTOR_ELT(ans,i+noelem*1,R_NaString);
			}
			++i;
		}
		UNPROTECT(1);

		edf_close_file(ef);
		return(ans);
	}
	return(R_NilValue);
}


SEXP get_trial_count(SEXP filename)
{

    int check(0);

    EDFFILE* ef = edf_open_file( as_string(filename), 0, 1, 0, &check);
    int id = edf_get_trial_count(ef);
    edf_close_file(ef);
    SEXP ret;
    ret = ScalarInteger(id);
  // edf_get_end_trial_identifier()
  // Rprintf("trial id is %i", id );
  return(ret);
}

SEXP get_trial_id(SEXP filename)
{

  int check(0);

  EDFFILE* ef = edf_open_file( as_string(filename), 0, 1, 0, &check);
  int ntrials = edf_get_trial_count(ef);
  char* tstart = edf_get_start_trial_identifier(ef);
  char* tend = edf_get_start_trial_identifier(ef);


  edf_set_trial_identifier(ef, "TRIALID", "TRIAL OK");

  // loop through trials


TRIAL* Header =new TRIAL;

RECORDINGS* data = new RECORDINGS[ntrials];

for(int iTrial=0; iTrial<ntrials; iTrial++)
{

  // navigating to the current trial
  int JumpResults= edf_jump_to_trial(ef, iTrial);

  // obtaining its header
  int GoodJump= edf_get_trial_header(ef, Header);
  //   mxSetFieldByNumber(mexTrials, iTrial, 0, ExportEDFInfo(Header));

  // clearing arrays
  //   SamplesClass.Reset();
  //   Events.clear();

  // samples/events data holders
  ALLF_DATA* CurrentData;
  int DataType;

  // getting data
  bool TrialIsOver= false;
  UINT32 CurrentTime;
  for(DataType= edf_get_next_data(ef); DataType!=NO_PENDING_ITEMS && !TrialIsOver; DataType= edf_get_next_data(ef))
  {
    // obtaining actual data
    CurrentData= edf_get_float_data(ef);
    switch(DataType)
    {
    case SAMPLE_TYPE:
      CurrentTime= CurrentData->fs.time;
      // AppendSample(CurrentData->fs);
      break;
    case STARTPARSE:
    case ENDPARSE:
    case BREAKPARSE:
    case STARTBLINK :
    case ENDBLINK:
    case STARTSACC:
    case ENDSACC:
    case STARTFIX:
    case ENDFIX:
    case FIXUPDATE:
    case MESSAGEEVENT:
    case STARTSAMPLES:
    case ENDSAMPLES:
    case STARTEVENTS:
    case ENDEVENTS:
      CurrentTime= CurrentData->fe.sttime;
      if (CurrentTime>Header->endtime)
      {
        TrialIsOver= true;
        break;
      }
      // AppendEvent(CurrentData->fe);
      break;
    case BUTTONEVENT:
    case INPUTEVENT:
    case LOST_DATA_EVENT:
      CurrentTime= CurrentData->fe.sttime;
      if (CurrentTime>Header->endtime)
      {
        TrialIsOver= true;
        break;
      }
      // AppendEvent(CurrentData->fe);
      break;
    case RECORDING_INFO:
      CurrentTime= CurrentData->fe.time;
      //AppendRecordingInfo(CurrentData->rec);
      data[iTrial] = CurrentData->rec;
      break;
    default:
      CurrentTime= CurrentData->fe.time;
    }

    //mexPrintf("%d\n", CurrentTime);

    // end of trial check
    if (CurrentTime>Header->endtime)
      break;
  }
}

SEXP ans;
int nx(ntrials), ny(9);
PROTECT(ans = allocMatrix(REALSXP, nx, ny));
double* rans = REAL(ans);


UNPROTECT(1);
delete[] data;

  edf_close_file(ef);
//   SEXP ans;
//   ans = mkString(tstart);

  return(ans);
}


}
