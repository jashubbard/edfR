#include <string.h>


// NOTE Both of the following are obsolete now because we're using isnan defined below.
//#include <opt.h> //for NaN
//#define NaN 1e8

#ifndef isnan
bool isnan(double var)
{
  volatile double d = var;
  return d != d;
}
#endif


SEXP mkReal(float x)
{
	SEXP ans = PROTECT(Rf_allocVector(REALSXP,1));
	REAL(ans)[0] = x;
	UNPROTECT(1);
	return ans;
}

inline const char* as_string(SEXP x, int y=0){
	return CHAR(STRING_ELT(x, y));
}


inline const int is_member(const char* str, SEXP strvec){
	for(int i = 0; i < LENGTH(strvec); ++i)
		if(strcmp(str,as_string(strvec,i)) == 0)
			return i;
	return -1;
}

inline const int is_intmember(const int x, int* intvec, int length){
	for(int i = 0; i < length; ++i)
		if(intvec[i] == x)
			return i;
	return -1;
}

const int as_type(const char* str){
	if(strcmp(str,"STARTBLINK") == 0)
		return STARTBLINK;
	else if(strcmp(str,"STARTSACC") == 0)
		return STARTSACC;
	else if(strcmp(str,"STARTFIX") == 0)
		return STARTFIX;
	else if(strcmp(str,"STARTSAMPLES") == 0)
		return STARTSAMPLES;
	else if(strcmp(str,"STARTEVENTS") == 0)
		return STARTEVENTS;
	else if(strcmp(str,"STARTPARSE") == 0)
		return STARTPARSE;
	else if(strcmp(str,"ENDBLINK") == 0)
		return ENDBLINK;
	else if(strcmp(str,"ENDSACC") == 0)
		return ENDSACC;
	else if(strcmp(str,"ENDFIX") == 0)
		return ENDFIX;
	else if(strcmp(str,"ENDSAMPLES") == 0)
		return ENDSAMPLES;
	else if(strcmp(str,"ENDEVENTS") == 0)
		return ENDEVENTS;
	else if(strcmp(str,"ENDPARSE") == 0)
		return ENDPARSE;
	else if(strcmp(str,"FIXUPDATE") == 0)
		return FIXUPDATE;
	else if(strcmp(str,"BREAKPARSE") == 0)
		return BREAKPARSE;
	else if(strcmp(str,"BUTTONEVENT") == 0)
		return BUTTONEVENT;
	else if(strcmp(str,"INPUTEVENT") == 0)
		return INPUTEVENT;
	else if(strcmp(str,"MESSAGEEVENT") == 0)
		return MESSAGEEVENT;
	else if(strcmp(str,"SAMPLE_TYPE") == 0)
		return SAMPLE_TYPE;
	else if(strcmp(str,"RECORDING_INFO") == 0)
		return RECORDING_INFO;
	else
		return NO_PENDING_ITEMS;
}

int* as_typelist(SEXP _typelist, int len){
	int* typelist = new int[len];
	for(int i = 0; i < len; ++i)
		typelist[i] = as_type(as_string(_typelist,i));
	return typelist;
}


inline void fill_samples(SEXP fields, double* rans, const int& size, FSAMPLE* data)
{
	int col(-1);

	col = is_member("time",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].time;

	col = is_member("flags",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].flags;

	col = is_member("pxL",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_LEFT_WARNING || data[j].px[0]==MISSING_DATA || isnan(data[j].px[0])) ? NA_REAL : data[j].px[0];

	col = is_member("pxR",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_RIGHT_WARNING || data[j].px[1]==MISSING_DATA || isnan(data[j].px[1])) ? NA_REAL : data[j].px[1];

	col = is_member("pyL",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_LEFT_WARNING || data[j].py[0]==MISSING_DATA || isnan(data[j].py[0])) ? NA_REAL : data[j].py[0];

	col = is_member("pyR",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_RIGHT_WARNING || data[j].py[1]==MISSING_DATA || isnan(data[j].py[1])) ? NA_REAL : data[j].py[1];

	col = is_member("hxL",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_LEFT_WARNING || data[j].hx[0]==MISSING_DATA || isnan(data[j].hx[0])) ? NA_REAL : data[j].hx[0];

	col = is_member("hxR",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_RIGHT_WARNING || data[j].hx[1]==MISSING_DATA || isnan(data[j].hx[1])) ? NA_REAL : data[j].hx[1];

	col = is_member("hyL",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_LEFT_WARNING || data[j].hy[0]==MISSING_DATA || isnan(data[j].hy[0])) ? NA_REAL : data[j].hy[0];

	col = is_member("hyR",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_RIGHT_WARNING || data[j].hy[1]==MISSING_DATA || isnan(data[j].hy[1])) ? NA_REAL : data[j].hy[1];

	col = is_member("paL",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_LEFT_WARNING || data[j].pa[0]==MISSING_DATA || isnan(data[j].pa[0])) ? NA_REAL : data[j].pa[0];

	col = is_member("paR",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_RIGHT_WARNING || data[j].pa[1]==MISSING_DATA || isnan(data[j].pa[1])) ? NA_REAL : data[j].pa[1];

	col = is_member("gxL",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_LEFT_WARNING || data[j].gx[0]==MISSING_DATA || isnan(data[j].gx[0])) ? NA_REAL : data[j].gx[0];

	col = is_member("gxR",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_RIGHT_WARNING || data[j].gx[1]==MISSING_DATA || isnan(data[j].gx[1])) ? NA_REAL : data[j].gx[1];

	col = is_member("gyL",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_LEFT_WARNING || data[j].gy[0]==MISSING_DATA || isnan(data[j].gy[0])) ? NA_REAL : data[j].gy[0];

	col = is_member("gyR",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
          rans[j + size*col] = (data[j].errors&CR_LOST_RIGHT_WARNING || data[j].gy[1]==MISSING_DATA || isnan(data[j].gy[1])) ? NA_REAL : data[j].gy[1];

	col = is_member("rx",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].rx;

	col = is_member("ry",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].ry;

	/** New version of edfapi doesn't have status flags anymore.
	col = is_member("status",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].status;
	*/

	col = is_member("errors",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].errors;
}

inline void fill_recordings(SEXP fields, double* rans, const int& size, RECORDINGS* data)
{
	int col(-1);

	col = is_member("time",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].time;

	col = is_member("state",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].state;

	col = is_member("record_type",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].record_type;

	col = is_member("pupil_type",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].pupil_type;

	col = is_member("recording_mode",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].recording_mode;

	col = is_member("filter_type",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].filter_type;

	col = is_member("sample_rate",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].sample_rate;

	col = is_member("pos_type",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].pos_type;

	col = is_member("eye",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = ((data[j].state) ? data[j].eye : 0);
}

inline void fill_events(SEXP fields, double* rans, const int& size, FEVENT* data)
{
	int col(-1);

	col = is_member("time",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].time;

	col = is_member("type",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].type;

	col = is_member("read",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].read;

	col = is_member("eye",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].eye;

	col = is_member("sttime",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].sttime;

	col = is_member("entime",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].entime;

	col = is_member("hstx",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].hstx;

	col = is_member("hsty",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].hsty;

	col = is_member("gstx",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].gstx;

	col = is_member("gsty",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].gsty;

	col = is_member("sta",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].sta;

	col = is_member("henx",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].henx;

	col = is_member("heny",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].heny;

	col = is_member("genx",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].genx;

	col = is_member("geny",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].geny;

	col = is_member("ena",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].ena;

	col = is_member("havx",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].havx;

	col = is_member("havy",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].havy;

	col = is_member("gavx",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].gavx;

	col = is_member("gavy",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].gavy;

	col = is_member("ava",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].ava;

	col = is_member("avel",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].avel;

	col = is_member("pvel",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].pvel;

	col = is_member("svel",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].svel;

	col = is_member("evel",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].evel;

	col = is_member("supd_x",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].supd_x;

	col = is_member("eupd_x",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].eupd_x;

	col = is_member("supd_y",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].supd_y;

	col = is_member("eupd_y",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].eupd_y;

	col = is_member("status",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].status;

	col = is_member("flags",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].flags;

	col = is_member("input",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].input;

	col = is_member("buttons",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].buttons;

	col = is_member("parsedby",fields);
	if(col != -1)
		for(int j = 0; j < size; ++j)
			rans[j + size*col] = data[j].parsedby;
}

