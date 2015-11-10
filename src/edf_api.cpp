// this code is mostly from Titus von der Malsburg: http://pages.ucsd.edu/~tvondermalsburg
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

  // begin part written by Jason Hubbard, using code from Alexander Pastukhov's edfImport matlab package
  // (http://kobi.nat.uni-magdeburg.de/edfImport) along with Titus von der Malsburg's code above

  SEXP get_trial_count(SEXP filename)
  {
    // quickly get the number of trials from the edf
    int check(0);
    EDFFILE* ef = edf_open_file( as_string(filename), 0, 1, 0, &check);
    int count = edf_get_trial_count(ef);
    edf_close_file(ef);
    SEXP ans = ScalarInteger(count);
    return(ans);
  }

  SEXP get_trial_data(SEXP filename,SEXP evtfields,SEXP sampfields,SEXP getsamples)
  {

    // load the EDF
    int check(0);
    EDFFILE* ef = edf_open_file( as_string(filename), 0, 1,asInteger(getsamples), &check);


    // output the number of trials and elements
    int ntrials = edf_get_trial_count(ef);
    int noelem = edf_get_element_count(ef);

    Rprintf("Found %i trials\n",ntrials);

    if (asLogical(getsamples))
      Rprintf("Loading Events and Samples (%i)...\n",noelem);
    else
      Rprintf("Loading Events (%i)....\n",noelem);

    // set the trial identifier (these are the defaults)
    edf_set_trial_identifier(ef, "TRIALID", "TRIAL OK");

    // data structures for the different pieces of information
    SEXP allevents, allsamp,allheaders,allmsg,alldata;
    PROTECT(allevents=allocVector(VECSXP,ntrials));
    PROTECT(allmsg=allocVector(VECSXP,ntrials));
    PROTECT(allsamp=allocVector(VECSXP,ntrials));
    PROTECT(alldata=allocVector(VECSXP,4));
    PROTECT(allheaders=allocMatrix(VECSXP, ntrials,4));

    // recording information for whole trial
    TRIAL* Header = new TRIAL;

    for(int iTrial=0; iTrial<ntrials; iTrial++)
    {

      // navigating to the current trial
      int JumpResults= edf_jump_to_trial(ef, iTrial);

      // obtaining its header
      int GoodJump= edf_get_trial_header(ef, Header);

      //save recording times for all trials
      SET_VECTOR_ELT(allheaders, iTrial+ntrials*0,ScalarInteger(iTrial+1));
      SET_VECTOR_ELT(allheaders, iTrial+ntrials*1,ScalarInteger(Header->starttime));
      SET_VECTOR_ELT(allheaders, iTrial+ntrials*2,ScalarInteger(Header->endtime));
      SET_VECTOR_ELT(allheaders, iTrial+ntrials*3,ScalarInteger(Header->duration));

      // samples/events data holders
      ALLF_DATA* CurrentData;
      int DataType;
      UINT32 CurrentTime;
      bool TrialIsOver= false;

      //first we need to get the number of events, samples, and recordings for the trial
      int nsamp(0),nevt(0);
      //loop through all data of this trial
      for(DataType= edf_get_next_data(ef); DataType!=NO_PENDING_ITEMS && !TrialIsOver; DataType= edf_get_next_data(ef))
      {

        CurrentData= edf_get_float_data(ef);
        switch(DataType){
        case SAMPLE_TYPE:
//           CurrentTime= CurrentData->fe.sttime;
//           if (CurrentTime>Header->endtime)
//           {
//             TrialIsOver= true;
//             break;
//           }
          nsamp++;
          break;
        case STARTPARSE:
        case ENDPARSE:
        case BREAKPARSE:
        case STARTBLINK:
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

          nevt++;
          break;
        case LOST_DATA_EVENT:
          CurrentTime= CurrentData->fe.sttime;
          if (CurrentTime>Header->endtime)
          {
            TrialIsOver= true;
            break;
          }
          //       case RECORDING_INFO:
          //         nrec++;
        }

      }

      // navigate back to the current trial
      JumpResults= edf_jump_to_trial(ef, iTrial);

      // obtaining its header (again)
      GoodJump= edf_get_trial_header(ef, Header);

      //structures for holding events, samples, etc.
      FEVENT* tevents = new FEVENT[nevt];
      FSAMPLE* tsamp = new FSAMPLE[nsamp];
      char* CurrentMsg;
      int ecount(0),rcount(0),scount(0);
      DataType=0;
      TrialIsOver=false;

      // structure for holding messages (since they're strings)
      SEXP tmsg;
      PROTECT(tmsg=allocVector(VECSXP, nevt));

      // loop through events/samples again
      for(DataType= edf_get_next_data(ef); DataType!=NO_PENDING_ITEMS && !TrialIsOver; DataType= edf_get_next_data(ef))
      {

        // obtaining actual data
        CurrentData= edf_get_float_data(ef);
        switch(DataType)
        {
        case SAMPLE_TYPE:
          CurrentTime= CurrentData->fs.time;
//           // break if we're past the trial time
//           if (CurrentTime>Header->endtime)
//           {
//             TrialIsOver= true;
//             break;
//           }
          // store the actual sample data
          tsamp[scount]=CurrentData->fs;
          scount++;
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
          // if we're past the current trial, then break
          if (CurrentTime>Header->endtime)
          {
            TrialIsOver= true;
            break;
          }
          // because messages are strings, we handle them separately
          if (DataType==MESSAGEEVENT)
            SET_VECTOR_ELT(tmsg,ecount,mkString(&(CurrentData->fe.message->c)));

          // store the event data
          tevents[ecount]=CurrentData->fe;
          ecount++;
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
          break;
        case RECORDING_INFO:
          CurrentTime= CurrentData->fe.time;

          break;
        default:
          CurrentTime= CurrentData->fe.time;
        }

        // end of trial check
        if (CurrentTime>Header->endtime)
          break;
      }

      //take events structure and turn into a matrix
      int nx(nevt), ny(LENGTH(evtfields)+1);
      SEXP events;
      PROTECT(events = allocMatrix(REALSXP, nx, ny));
      double* revents = REAL(events);
      fill_events(evtfields,revents,nevt,tevents);

      // now add a trial number column to the end
      for(int i=0;i<nevt;i++)
        revents[i+nevt*LENGTH(evtfields)]=iTrial+1;

      //do the same for samples (if you get them)
      if (asLogical(getsamples))
      {
        int nx(nsamp), ny(LENGTH(sampfields)+1);
        SEXP samples;
        PROTECT(samples = allocMatrix(REALSXP, nx, ny));
        double* rsamples = REAL(samples);
        fill_samples(sampfields,rsamples,nsamp,tsamp);

        for(int i=0;i<nsamp;i++)
          rsamples[i+nsamp*LENGTH(sampfields)]=iTrial+1;

        UNPROTECT(1);

        // at each trial loop, add the sample data
        SET_VECTOR_ELT(allsamp,iTrial,samples);
      }

      // at each trial loop, add the event and message data
      SET_VECTOR_ELT(allevents,iTrial,events);
      SET_VECTOR_ELT(allmsg,iTrial,tmsg);
      UNPROTECT(2);

    }

    // once we have all our header, event, message, and sample data
    // add them all to 1 big vector to give to R
    SET_VECTOR_ELT(alldata,0,allheaders);
    SET_VECTOR_ELT(alldata,1,allevents);
    SET_VECTOR_ELT(alldata,2,allmsg);
    SET_VECTOR_ELT(alldata,3,allsamp);

    edf_close_file(ef);

    UNPROTECT(5);
    return(alldata);
  }


}
