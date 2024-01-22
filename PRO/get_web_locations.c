

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

static int les_linje2();
static int parse_line_usgs();
static int parse_line_emsc();
static int parse_line_isc();
static int exists();
static void S_REC();
static void NOWTIME();
static int read_parameter(int argcount, char **argvec);
static void help (void);

char  *topdir      = 0;                       // top directory SEISAN
char  command[500];
char  newcom[500];
char  maglin[200];
char  complete[500];
char  dum[200];
int   ret;
int   cmd=0;
char  ID[25];
char  TIME[25];
char  DATE[25];
char  LAT[25];
float LATF;
char  LON[25];
float LONF;
char  DEP[25];
float DEPF;
char  AG[25];
char  AGENCY[25];
char  MTYP[25];
char  MT[10];
char  MAG[25];
float MAGF;
char  WHERE[250];
char  snam[200];
char  s_fullpath[200];
int   FORMAT;
int   loop_forever=0;
int   ival;
int   tstart;
int   tstop;
int   diff;
int   web_agency=0;
int   form=1;
char  fmt[30];
char  starttime[40];
char  endtime[40];
char  minmag[30];
char  mindep[30];
char  maxdep[30];
char  dbase[10];
char  srt_year[10];
char  srt_month[10];
char  srt_day[10];
char  srt_time[10];
char  end_year[10];
char  end_month[10];
char  end_day[10];
char  end_time[10];
char  shr[5];
char  smn[5];
char  ssk[5];
char  ehr[5];
char  emn[5];
char  esk[5];

float val;
int   scounter=0;
DIR   *dirp; 
struct dirent *direntp; 


time_t current_time;
time_t time_start;
time_t time_end;


int main(int argc, char **argv)
{
  FILE *sfile;
  FILE *inp;
  FILE *cop;
  int  br1;
  int  i,j,tel;
  char record[600];
  int  cnt=0; 
  char dummy[200];
  char du[30];
  char year[5];
  char month[5];
  char day[5];
  char hour[5];
  char minute[5];
  char sec[5];
  char seconds[10]; 
  char events_p[200];
  char database[20];
  char spath[200];  
  char idsfile[20];
  char idnumber[20]; 
  char oldid[20];
  int  yr;
  int  mn;
  int  dy;
  int  ho;
  int  mi;
  int  sc;
  int  retur;
  int  ut; 
  
  
  char lin[500];
  
  char * line = NULL;
  size_t len = 0;
  ssize_t read;  


  
  char buffer[26];
  struct tm* tm_info;  
  char* c_time_string;

  FORMAT=5;

//------------------------------------------------------------------------
// set path to SEISAN_TOP as default
//------------------------------------------------------------------------
  topdir = (char*)getenv("SEISAN_TOP");
  if(topdir)
  {
    printf("SEISAN_TOP..................................: %s\n", topdir);    
  }else{
    printf("SEISAN_TOP not defined.\n");
    printf("Install SEISAN !\n");
    exit(0);    
  }
  sprintf(starttime,"                              ");
  sprintf(endtime,"                             ");

//-------------------------------------------------------------------------
// Process command line parameters
//-------------------------------------------------------------------------
  if (read_parameter(argc, argv) < 0)
  {
    printf("GET_WEB_LOCATIONS: Parameter processing failed\n\n");
    printf("                   Try '-h' for detailed help\n");
    exit(0); 
  }
  
  if(FORMAT == 5)
  {
    sprintf(fmt,"text");
  }else{
    printf("Format not supported !\n");
    exit(0);
  }


for(;;)
{
 
  if(loop_forever == 1)
  {
    current_time = time(NULL);
    time_start = current_time - tstart*60;//the time 10 minutes ago is 10*60
    time_end = current_time - tstop*60;
  
    c_time_string = ctime(&time_start);//convert the time tenMinutesAgo into a string format in the local time format
    printf("\n\n\n");

//  printf("The time 10 minutes ago in seconds from the epoch is: %i\n", (int)time_start);
//  printf("The time 10 minutes ago from the epoch in the local time format is: %s\n", c_time_string);

//    time(&time_start);
//    tm_info = localtime(&time_start);
    tm_info = gmtime(&time_start);    
    strftime(buffer, 26, "%Y-%m-%dT%H:%M:%S", tm_info);
    printf("start_time: %s\n",buffer);
    sprintf(starttime,"%s",buffer);
    
//    tm_info = localtime(&time_end);
    tm_info = gmtime(&time_end);    
    strftime(buffer, 26, "%Y-%m-%dT%H:%M:%S", tm_info);
    printf("end_time  : %s\n",buffer);
    sprintf(endtime,"%s",buffer);    
  }
  
printf("web_agency: %d\n",web_agency);
  switch(web_agency)
  {
    case 0:
    sprintf(command,"wget -T 1 -O get_web_locations_USGS.txt %chttps://earthquake.usgs.gov/fdsnws/event/1/query?format=%s&starttime=%s&endtime=%s&minmagnitude=%s&mindepth=%s&maxdepth=%s%c 2>request.log ",0x22,fmt,starttime,endtime,minmag,mindep,maxdep,0x22);  
        
    break;
    case 1:
    sprintf(command,"wget -T 1 -O get_web_locations_EMSC.txt %chttp://www.seismicportal.eu/fdsnws/event/1/query?limit=1000&format=%s&start=%s&end=%s&minmag=%s&mindepth=%s&maxdepth=%s%c 2>request.log ",0x22,fmt,starttime,endtime,minmag,mindep,maxdep,0x22);        
    break;
    case 2:
//    printf("%s  %s\n",starttime,endtime);
/*        
    srt_year[0]=starttime[0];
    srt_year[1]=starttime[1];
    srt_year[2]=starttime[2];
    srt_year[3]=starttime[3];
    srt_year[4]='\0';
    srt_month[0]=starttime[5];
    srt_month[1]=starttime[6];
    srt_month[2]='\0';
    srt_day[0]=starttime[8];
    srt_day[1]=starttime[9];
    srt_day[2]='\0';
    srt_time[0]=starttime[11];
    srt_time[1]=starttime[12];
    srt_time[2]=starttime[13];
    srt_time[3]=starttime[14];
    srt_time[4]=starttime[15];
    srt_time[5]=starttime[16];
    srt_time[6]=starttime[17];
    srt_time[7]=starttime[18];     
    srt_time[8]='\0';
    
    shr[0]=starttime[11];
    shr[1]=starttime[12];
    shr[2]='\0';
    smn[0]=starttime[14];
    smn[1]=starttime[15];
    smn[2]='\0';
    ssk[0]=starttime[17];
    ssk[1]=starttime[18];
    ssk[2]='\0';
    
    
    end_year[0]=endtime[0];
    end_year[1]=endtime[1];
    end_year[2]=endtime[2];
    end_year[3]=endtime[3];
    end_year[4]='\0';
    end_month[0]=endtime[5];
    end_month[1]=endtime[6];
    end_month[2]='\0';
    end_day[0]=endtime[8];
    end_day[1]=endtime[9];
    end_day[2]='\0';
    end_time[0]=endtime[11];
    end_time[1]=endtime[12];
    end_time[2]=endtime[13];
    end_time[3]=endtime[14];
    end_time[4]=endtime[15];
    end_time[5]=endtime[16];
    end_time[6]=endtime[17];
    end_time[7]=endtime[18];     
    end_time[8]='\0'; 
    
    ehr[0]=endtime[11];
    ehr[1]=endtime[12];
    ehr[2]='\0';
    emn[0]=endtime[14];
    emn[1]=endtime[15];
    emn[2]='\0';
    esk[0]=endtime[17];
    esk[1]=endtime[18];
    esk[2]='\0';    
*/
  
//  sprintf(command, "wget -T 10 -O get_web_locations.txt %chttp://www.isc.ac.uk/fdsnws/event/1/query?starttime=2011-01-08T00:00:00&endtime=2011-01-09T00:00:00&catalog=ISC&format=isf&mindepth=0&maxdepth=800&minmag=5.0%c 2>request.log",0x22,0x22);  
  
  sprintf(command, "wget -T 10 -O get_web_locations.txt %chttp://www.isc.ac.uk/fdsnws/event/1/query?starttime=%s&endtime=%s&catalog=ISC&format=isf&mindepth=%s&maxdepth=%s&minmag=%s%c 2>request.log",0x22,starttime,endtime,mindep,maxdep,minmag,0x22); 
  
  
    break;
  }

//  printf("%s\n",command);
  
  
  
//  printf("\n\n");
  
  ret = system(command); 
  
  
  sleep(10);

  if(web_agency == 0)
  {
    if ((sfile = fopen ("get_web_locations_USGS.txt", "r")) == NULL)
    {
      printf("GET_WEB_LOCATIONS: Can't open get_web_locations_USGS.txt file");
      return(-1);
    }
  }
  
  if(web_agency == 1)
  {
    if ((sfile = fopen ("get_web_locations_EMSC.txt", "r")) == NULL)
    {
      printf("GET_WEB_LOCATIONS: Can't open get_web_locations_EMSC.txt file");
      return(-1);
    }  
  }
  
if(web_agency == 0 || web_agency == 1)
{
  tel=0;   
  while(fgets(lin,300,sfile)!=NULL)
  {
//    printf("%s\n", lin);
    
    sprintf(record,"%s",lin);
    tel++;
    if(tel > 1)
    {
      switch(web_agency)
      {
          case 0:
          parse_line_usgs(record);
          break;
          case 1:
          parse_line_emsc(record);
          break;

      }
      for(i=0;i<200;i++)
      {
        snam[i]='\0';
        s_fullpath[i]='\0';
      } 
      sscanf(TIME,"%4d",&yr);    
      sprintf(year,"%4d",yr);    
      sscanf(TIME,"%5s%2s",dummy,month);
      sscanf(TIME,"%8s%2s",dummy,day);
      sscanf(TIME,"%11s%2s",dummy,hour);
      sscanf(TIME,"%14s%2s",dummy,minute);
      sscanf(TIME,"%17s%2s",dummy,sec);
      sscanf(TIME,"%20s%3s",dummy,seconds);    
      sscanf(year,"%d",&yr);
      sscanf(month,"%d",&mn);
      sscanf(day,"%d",&dy);
      sscanf(hour,"%d",&ho);
      sscanf(minute,"%d",&mi);
      sscanf(sec,"%d",&sc);
      sprintf(snam,"%2s-%2s%2s-%2sD.S%4s%2s",day,hour,minute,sec,year,month);  // s-file name
      for(i=0;i<19;i++)                 // replace ' ' with'0'
        if(snam[i]==' ')snam[i]='0';

#ifdef __WIN32__

      sprintf(database,"%s",dbase);
      sprintf(spath,"\%s\\REA",topdir);
      sprintf(events_p,"%s\\%s",spath,database);
      sprintf(dummy,"%s\\%s\\%s",events_p,year,month);
      printf("Create_Sfile_P DUMMY................: %s\n",dummy);
      dirp=opendir(dummy);
      if(dirp == NULL)
      {
        sprintf(command,"mkdir %s",dummy);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      } 
      closedir(dirp);      
//--------------------------------------------
// make full path for S-file
//--------------------------------------------
      sprintf(s_fullpath,"%s\\%s",dummy,snam);
//      printf("%s\n",s_fullpath);
      retur=exists(s_fullpath);   // new
      printf("retur: %d\n",retur);
#else
//      printf("Linux\n");
      sprintf(database,"%s",dbase);
      sprintf(spath,"%s/REA",topdir);

      dirp=opendir(spath);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",spath);
        sprintf(command,"mkdir %s",spath);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      }   
      closedir(dirp);
      sprintf(events_p,"%s/%s",spath,database);

      dirp=opendir(events_p);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",events_p);
        sprintf(command,"mkdir %s",events_p);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      }   
      closedir(dirp);
      sprintf(dummy,"%s/%s",events_p,year);

      dirp=opendir(dummy);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",dummy);
        sprintf(command,"mkdir %s",dummy);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      }
      closedir(dirp);
      sprintf(spath,"%s/%s",dummy,month);
      sprintf(dummy,"%s",spath);
      dirp=opendir(dummy);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",dummy);
        sprintf(command,"mkdir %s",dummy);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      } 
      closedir(dirp);
//--------------------------------------------
// make full path for S-file
//--------------------------------------------
      sprintf(s_fullpath,"%s/%s",dummy,snam);
      retur=exists(s_fullpath);   // new
//      printf("retur: %d\n",retur);  
#endif

      for(;;)
      {
        ut = 0;

        retur=exists(s_fullpath);  // s-file with same name ?
//        printf("HERE: %s  retur: %d\n",s_fullpath,retur);        
        if(retur == 1)
        {
          if ((inp = fopen (s_fullpath, "r")) == NULL)
          {
            printf("GET_WEB_LOCATIONS: Can't open %s\n",s_fullpath);
            return(-1);
          }
          fgets(lin,200,inp);
          fgets(lin,200,inp);
          fclose(inp);
          sscanf(lin,"%s %s %s",du,du,oldid);
          ret=strcmp(oldid,ID);
        
          if(ret != 0)
          {
      
            sc=sc+1;
            if(sc > 59)
            {
              sc = 0;
              mi = mi +1;
              if(mi > 59)
              {
	        mi = 0;
	        ho = ho+1;
              }
            }
            sprintf(hour,"%2d",ho);
            sprintf(minute,"%2d",mi);
            sprintf(sec,"%2d",sc);
            sprintf(idsfile,"ID:%4s%2s%2s%2s%2s%2sd",year,month,day,hour,minute,sec);
            sprintf(snam,"%2s-%2s%2s-%2sD.S%4s%2s",day,hour,minute,sec,year,month);
            for(i=0;i<19;i++)
              if(snam[i]==' ')snam[i]='0';

            sprintf(s_fullpath,"%s/%s",dummy,snam);

            ut = 1;
            break;
          }else{
            sprintf(idsfile,"ID:%4s%2s%2s%2s%2s%2s",year,month,day,hour,minute,sec);
            sprintf(idnumber,"%s",ID);              
            ut=1;
            break;
          }
        }else{
          sprintf(idsfile,"ID:%4s%2s%2s%2s%2s%2s",year,month,day,hour,minute,sec);
          sprintf(idnumber,"%s",ID);
          ut = 1;
        }
        if(ut == 1)
          break;
      }

      S_REC(year,month,day,hour,minute, sec, dummy,s_fullpath,snam,idsfile,idnumber);  

    }
  }  // eof while
  fclose(sfile);
  
}else{                           // ISC-ROUTINE
  tel=0;   
  while(fgets(lin,300,sfile)!=NULL)
  {
//    printf("%s\n", lin);
    
    sprintf(record,"%s",lin);
    tel++;
//    if(tel >= 3)
    if(lin[0] == 'E' && lin[1] == 'v')    
    {
//      printf("%s\n",lin);              // Locality
      sprintf(WHERE,"%s",lin);
      for(i=0;i<100;i++)
      {
        if(WHERE[i] == 0xa)
            WHERE[i]='\0';
      }
    
      fgets(lin,300,sfile);            // top title line
      fgets(lin,300,sfile);            // date+time+etc
      sprintf(record,"%s",lin);
      
      parse_line_isc(record);
      
      fgets(lin,300,sfile);            // blank line
      fgets(lin,300,sfile);            // title magnitude
//      printf("%s\n",lin);
      fgets(lin,300,sfile);             // magnitude line
      sscanf(lin,"%s %s",MTYP,MAG);
//      printf("%s  %s\n",MTYP,MAG);
      
      
      for(i=0;i<200;i++)
      {
        snam[i]='\0';
        s_fullpath[i]='\0';
      }
//      printf("DATE: %s   TIME: %s\n",DATE,TIME);
      DATE[10]='-';
      DATE[11]=TIME[0];
      DATE[12]=TIME[1];
      DATE[13]=TIME[2];
      DATE[14]=TIME[3];
      DATE[15]=TIME[4];
      DATE[16]=TIME[5];
      DATE[17]=TIME[6];
      DATE[18]=TIME[7];
      DATE[19]=TIME[8];
      DATE[20]=TIME[9];
      DATE[21]=TIME[10];
      DATE[22]='\0';
      sprintf(TIME,"%s",DATE);
//      printf("%s\n",TIME);
      
      
      sscanf(TIME,"%4d",&yr);    
      sprintf(year,"%4d",yr);    
      sscanf(TIME,"%5s%2s",dummy,month);
      sscanf(TIME,"%8s%2s",dummy,day);
      
      sscanf(TIME,"%11s%2s",dummy,hour);
      sscanf(TIME,"%14s%2s",dummy,minute);
      sscanf(TIME,"%17s%2s",dummy,sec);
      sscanf(TIME,"%20s%3s",dummy,seconds);    
      sscanf(year,"%d",&yr);
      sscanf(month,"%d",&mn);
      sscanf(day,"%d",&dy);
      sscanf(hour,"%d",&ho);
      sscanf(minute,"%d",&mi);
      sscanf(sec,"%d",&sc);      
      
      sprintf(snam,"%2s-%2s%2s-%2sD.S%4s%2s",day,hour,minute,sec,year,month);  // s-file name
      for(i=0;i<19;i++)                 // replace ' ' with'0'
        if(snam[i]==' ')snam[i]='0';
        
//        printf("%s\n",snam);
      

#ifdef __WIN32__

      sprintf(database,"%s",dbase);
      sprintf(spath,"\%s\\REA",topdir);
      sprintf(events_p,"%s\\%s",spath,database);
      sprintf(dummy,"%s\\%s\\%s",events_p,year,month);
      printf("Create_Sfile_P DUMMY................: %s\n",dummy);
      dirp=opendir(dummy);
      if(dirp == NULL)
      {
        sprintf(command,"mkdir %s",dummy);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      } 
      closedir(dirp);      
//--------------------------------------------
// make full path for S-file
//--------------------------------------------
      sprintf(s_fullpath,"%s\\%s",dummy,snam);
//      printf("%s\n",s_fullpath);
      retur=exists(s_fullpath);   // new
      printf("retur: %d\n",retur);
#else
//      printf("Linux\n");
      sprintf(database,"%s",dbase);
      sprintf(spath,"%s/REA",topdir);

      dirp=opendir(spath);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",spath);
        sprintf(command,"mkdir %s",spath);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      }   
      closedir(dirp);
      sprintf(events_p,"%s/%s",spath,database);

      dirp=opendir(events_p);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",events_p);
        sprintf(command,"mkdir %s",events_p);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      }   
      closedir(dirp);
      sprintf(dummy,"%s/%s",events_p,year);

      dirp=opendir(dummy);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",dummy);
        sprintf(command,"mkdir %s",dummy);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      }
      closedir(dirp);
      sprintf(spath,"%s/%s",dummy,month);
      sprintf(dummy,"%s",spath);
      dirp=opendir(dummy);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",dummy);
        sprintf(command,"mkdir %s",dummy);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      } 
      closedir(dirp);
//--------------------------------------------
// make full path for S-file
//--------------------------------------------
      sprintf(s_fullpath,"%s/%s",dummy,snam);
      retur=exists(s_fullpath);   // new
//      printf("retur: %d\n",retur);  
#endif

      for(;;)
      {
        ut = 0;

        retur=exists(s_fullpath);  // s-file with same name ?
//        printf("HERE: %s  retur: %d\n",s_fullpath,retur);        
        if(retur == 1)
        {
          if ((inp = fopen (s_fullpath, "r")) == NULL)
          {
            printf("GET_WEB_LOCATIONS: Can't open %s\n",s_fullpath);
            return(-1);
          }
          fgets(lin,200,inp);
          fgets(lin,200,inp);
          fclose(inp);
          sscanf(lin,"%s %s %s",du,du,oldid);
          ret=strcmp(oldid,ID);
        
          if(ret != 0)
          {
      
            sc=sc+1;
            if(sc > 59)
            {
              sc = 0;
              mi = mi +1;
              if(mi > 59)
              {
	        mi = 0;
	        ho = ho+1;
              }
            }
            sprintf(hour,"%2d",ho);
            sprintf(minute,"%2d",mi);
            sprintf(sec,"%2d",sc);
            sprintf(idsfile,"ID:%4s%2s%2s%2s%2s%2sd",year,month,day,hour,minute,sec);
            sprintf(snam,"%2s-%2s%2s-%2sD.S%4s%2s",day,hour,minute,sec,year,month);
            for(i=0;i<19;i++)
              if(snam[i]==' ')snam[i]='0';

            sprintf(s_fullpath,"%s/%s",dummy,snam);

            ut = 1;
            break;
          }else{
            sprintf(idsfile,"ID:%4s%2s%2s%2s%2s%2s",year,month,day,hour,minute,sec);
            sprintf(idnumber,"%s",ID);              
            ut=1;
            break;
          }
        }else{
          sprintf(idsfile,"ID:%4s%2s%2s%2s%2s%2s",year,month,day,hour,minute,sec);
          sprintf(idnumber,"%s",ID);
          ut = 1;
        }
        if(ut == 1)
          break;
      }

      S_REC(year,month,day,hour,minute, sec, dummy,s_fullpath,snam,idsfile,idnumber);  

    }
  }  // eof while
  fclose(sfile);    
    
    
    
}
  
  
  
  if(loop_forever == 1)
  {
     diff=(tstart-tstop)*60;
     printf("sleep %d  %d\n",diff,diff/(60));
     sleep(diff);   
  }else{
    exit(0);
  }
  
  }    
    
    
}


void S_REC(year,month,day,hour,minute,seconds,dummy,fullpath,filename,idsfile,idnumber)
char year[];
char month[];
char day[];
char hour[];
char minute[];
char seconds[];
char dummy[];
char fullpath[];
char filename[];
char idsfile[];
char idnumber[];

{

  FILE    *sf;
  char buf[256];
  char comp[200];
  char dm1[200];
  char dm2[200];
  int hr1;
  int mn1;
  int sc1;
  int ms1;

  int i,l;
  int ch; 
  int cnt;
  int ret;
  int YR,MTH,DAY,HR,MIN;
  float SEC;
  int AR;
  int cyr;
  int cmon;
  int cday;
  int chrn;
  int cmin;
  int cisec;
//printf("SECONDS: %s\n",seconds);
  sscanf(year,"%d",&YR);
//printf("%s %d\n",year,YR);
  sscanf(month,"%d",&MTH);
//printf("%s %d\n",month,MTH);
  sscanf(day,"%d",&DAY);
//printf("%s %d\n",day,DAY);
  sscanf(hour,"%d",&HR);
//printf("%s %d\n",hour,HR);
  sscanf(minute,"%d",&MIN);
//printf("%s %d\n",minute,MIN);
  sscanf(seconds,"%f",&SEC);
//printf("%s %f\n",seconds,SEC);

//fclose(sf);
    sf=fopen(fullpath,"w");
    if(sf == NULL)
    {

      printf("GET_WEB_LOCATIONS: S_REC Can't open S-file: %s  SCOUNTER: %d\n",fullpath,scounter);
      perror(fullpath);
      exit(EXIT_FAILURE);      
//    for(i=0;i<52;i++)
//      printf("%2d %2x %c\n",i,fullpath[i],fullpath[i]);
      exit(0);
  }else{
    for(i=0;i<100;i++)
    buf[i]=' ';
//  printf("RTDET: S_REC.............................: %4d %2d%2d %2d%2d %4.1f\n",YR,MTH,DAY,HR,MIN,SEC);
    sscanf(LAT,"%f",&LATF);
    sscanf(LON,"%f",&LONF);
    sscanf(DEP,"%f",&DEPF);
    sscanf(MAG,"%f",&MAGF);
    sscanf(TIME,"%17s%6s",dummy,dm1);
    sscanf(dm1,"%f",&SEC);

    cnt=0;
    if(MTYP[0] == 'm' && MTYP[1] == 'b')
      cnt=1;
    if(MTYP[0] == 'M' && MTYP[1] == 'L')
      cnt=2;      
    if(MTYP[0] == 'm' && MTYP[1] == 'B')
      cnt=3;
    if(MTYP[0] == 'M' && MTYP[1] == 's')
      cnt=4;
    if(MTYP[0] == 'M' && MTYP[1] == 'S')
      cnt=5;  
    if(MTYP[0] == 'M' && MTYP[1] == 'W')
      cnt=6;  
    if(MTYP[0] == 'M' && MTYP[1] == 'c')
      cnt=7;  
    if(MTYP[0] == 'm' && MTYP[1] == 'w')
      cnt=8;
    if(MTYP[0] == 'w' && MTYP[1] == 'w')
      cnt=9; 
    if(MTYP[0] == 'w' && MTYP[1] == 'r')
      cnt=10;   
    if(MTYP[0] == 'm' && MTYP[1] == 'l')
      cnt=11;   
    if(MTYP[0] == 'm' && MTYP[1] == 'd')
      cnt=12;    
  
    switch(cnt)
    {
      case 0:
      MT[0]=' ';
      MT[1]='\0';
      break;      
      case 1:
      MT[0]='b';
      MT[1]='\0';
      break;
      case 2:
      MT[0]='L';
      MT[1]='\0';
      break;
      case 3:
      MT[0]='B';
      MT[1]='\0';
      break;
      case 4:
      MT[0]='s';
      MT[1]='\0';
      break;
      case 5:
      MT[0]='S';
      MT[1]='\0';
      break;
      case 6:
      MT[0]='W';
      MT[1]='\0';
      break;
      case 7:
      MT[0]='C';
      MT[1]='\0';
      break;
      case 8:
      MT[0]='W';
      MT[1]='\0';
      break;  
      case 9:
      MT[0]='W';
      MT[1]='\0';
      break;   
      case 10:
      MT[0]='W';
      MT[1]='\0';
      break;   
      case 11:
      MT[0]='L';
      MT[1]='\0';
      break; 
      case 12:
      MT[0]='C';
      MT[1]='\0';
      break;       
    }
//    printf(" %4d %2d%2d %2d%2d %4.1f R %7.3f%8.3f%5.1f  %s                       %4.1f%s%s\n",YR,MTH,DAY,HR,MIN,SEC,LATF,LONF,DEPF,AGENCY,MAGF,MT,AGENCY);  
    sprintf(buf," %4d %2d%2d %2d%2d %4.1f D %7.3f%8.3f%5.1f  %s                       %4.1f%s%s",YR,MTH,DAY,HR,MIN,SEC,LATF,LONF,DEPF,AGENCY,MAGF,MT,AGENCY); 
//    for(i=48;i<73;i++)
//      buf[i]=' ';
//buf[73]=' ';
    buf[79]='1';
    buf[80]='\0';
//    printf("LINE1\n");
//    for(i=0;i<82;i++)
//        printf("%3d %c %x\n",i,buf[i],buf[i]);
//    printf("%s\n",buf);
//    exit(0);
    fprintf(sf,"%s\n",buf);
  
// provider id line
  
    for(i=0;i<100;i++)
      buf[i]=' ';
    sprintf(buf," WEB event-id: %s",idnumber);
    for(i=0;i<40;i++)
    {
      if(buf[i] == '\0')
        buf[i] = ' ';
    }
    buf[79]='3';
    buf[80]='\0';  
    fprintf(sf,"%s\n",buf);   
  
    for(i=0;i<100;i++)
      buf[i]=' ';
    l=11;
    sprintf(buf," LOCALITY: ");
    for(i=0;i<90;i++)
    {
      if(WHERE[i] != '\0')
      {
        buf[l]=WHERE[i];
        l++;
      }else{
        break;
      }
      buf[79]='3';
      buf[80]='\0';
    }
    fprintf(sf,"%s\n",buf);  
  
    for(i=0;i<100;i++)
      buf[i]=' ';
//printf("In S_REC: FILENAME: %s\n",filename);
    sprintf(buf," %s",filename);

//  sprintf(buf," 2017-11-01-2329-16.USGS");

    for(i=0;i<100;i++)
      if(buf[i]=='\0')buf[i]=' ';
    buf[79]='6';
    buf[80]='\0';
//  fprintf(sf,"%s\n",buf);

    sprintf(dm1,"%4d",YR);
    sscanf(dm1,"%2c%d",dm2,&AR);

    for(i=0;i<100;i++)
      buf[i]=' ';
  
    NOWTIME(&cyr,&cmon,&cday,&chrn,&cmin,&cisec);  
  
    sprintf(buf," ACTION:NEW %2d-%2d-%2d %2d:%2d OP:SEIS STATUS:               %s",cyr,cmon,cday,chrn,cmin,idsfile);  
//  sprintf(buf," ACTION:NEW %2d-%2d-%2d %2d:%2d OP:SEIS STATUS:               %s",AR,MTH,DAY,HR,MIN,idsfile);  

    if(buf[12]==' ')buf[12]='0';
    if(buf[15]==' ')buf[15]='0';
    if(buf[18]==' ')buf[18]='0';
    if(buf[21]==' ')buf[21]='0';
    if(buf[24]==' ')buf[24]='0';
  
  

    for(i=0;i<100;i++)
      if(buf[i]=='\0')buf[i]=' ';
    for(i=59;i<73;i++)
      if(buf[i]==' ')buf[i]='0';
    buf[79]='I';
    buf[80]='\0';
    fprintf(sf,"%s\n",buf);                 
    sprintf(buf," STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO AIN AR TRES W  DIS CAZ7");
    fprintf(sf,"%s\n",buf);

// STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO AIN AR TRES W
// BER  BZ IP       2223  6.00    4
    scounter++;

    ret=fclose(sf);
//    printf("scounter: %d  fclose: %d\n",scounter,ret); 

  }
}

void NOWTIME(nyrn,nmon,nday,nhrn,nmin,nsek)
int *nyrn;
int *nmon;
int *nday;
int *nhrn;
int *nmin;
int *nsek;
{
  char dummy[256];

  int   iyrn;
  int   imon;
  int   iday;
  int   ihrn;
  int   imin;
  int   isec;
  int   i,j;
  int   BUFSECS;
  float  sek;
  char dum[50];

  time_t now = time(NULL);
  struct tm *now_s = localtime(&now);
  sprintf(dummy,"%d-%02d-%02d_%02d:%02d:%02d", 1900+now_s->tm_year, ++now_s ->tm_mon,now_s->tm_mday, now_s->tm_hour, now_s->tm_min,now_s->tm_sec);
    sscanf(dummy,"%4d%1c%2d%1c%2d%1c%2d%1c%2d%1c%2d",&iyrn,dum,&imon,dum,&iday,dum,&ihrn,dum,&imin,dum,&isec);
  iyrn=iyrn-2000;
  *nyrn=iyrn;
  *nmon=imon;
  *nday=iday;
  *nhrn=ihrn;
  *nmin=imin;
  *nmon=imon;
  *nsek=isec;
}

int exists(const char *fname)
{
  FILE *file;
  if (file = fopen(fname, "r"))
  {
    fclose(file);
    return 1;
  }
  return 0;
}


int parse_line_usgs(char line[])
{
  int k,j,n;
  int tel;
    
  for(k=0;k<25;k++)
  {
    ID[k]   = '\0'; 
    TIME[k] = '\0';
    LAT[k]  = '\0';
    LON[k]  = '\0';
    MTYP[k] = '\0';
    MAG[k]  = '\0';
    DEP[k]  = '\0';
    AG[k]   = '\0';
    AGENCY[k]  = '\0';
  }
  for(k=0;k<250;k++)
    WHERE[k] = '\0';
  
  for(k=0;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
      ID[k]=line[k];
    }else{
//      printf("ID.......: %s\n",ID);
      break;
    }
  }
//----------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 1)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      TIME[n]=line[k];

      n++;
    }else{
      break;
    }
  }
//  printf("TIME.....: %s\n",TIME);  

//------------------------------------------  

  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 2)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      LAT[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("LAT......: %s\n",LAT);  
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 3)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      LON[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("LON......: %s\n",LON);  
//------------------------------------------  
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 4)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      DEP[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("DEP......: %s\n",DEP);  
//------------------------------------------ 
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 6)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      AG[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("AG.......: %s  AG[0] = %c  AG[1] = %c AG[2] : %x\n",AG,AG[0],AG[1],AG[2]);
  if(n==2)
  {
    AGENCY[0] = ' ';
    AGENCY[1] = AG[0];
    AGENCY[2] = AG[1];
    AGENCY[3] = '\0';
  }
  if(AGENCY[0] == ' ' && AGENCY[1] == 'u' && AGENCY[2] == 's')
  {
    AGENCY[0]='U';
    AGENCY[1]='S';
    AGENCY[2]='G';
  }
//  printf("AGENCY: %s\n",AGENCY);
//  exit(0);
//------------------------------------------      
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 9)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      MTYP[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("MTYP.....: %s\n",MTYP);  
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 10)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      MAG[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("MAG......: %s\n",MAG);  
//------------------------------------------   
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 12)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
//  sprintf(WHERE," LOCALITY: ");
  for(k=j;k<600;k++)
  {
    if(line[k] != 0xa)
    {
//      printf("%c\n",line[k]);
      WHERE[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("WHERE....: %s\n",WHERE);  
//------------------------------------------   
}
int parse_line_emsc(char line[])
{
  int k,j,n;
  int tel;
    
  for(k=0;k<25;k++)
  {
    ID[k]   = '\0'; 
    TIME[k] = '\0';
    LAT[k]  = '\0';
    LON[k]  = '\0';
    MTYP[k] = '\0';
    MAG[k]  = '\0';
    DEP[k]  = '\0';
    AG[k]   = '\0';
    AGENCY[k]  = '\0';
  }
  for(k=0;k<250;k++)
    WHERE[k] = '\0';
  
  for(k=0;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
      ID[k]=line[k];
    }else{
//      printf("ID.......: %s\n",ID);
      break;
    }
  }
//----------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 1)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      TIME[n]=line[k];
      if(TIME[n]=='Z')
      {
        TIME[n]='\0';
        break;
      }
      n++;
    }else{
      break;
    }
  }
//  printf("TIME.....: %s\n",TIME);  

//------------------------------------------  

  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 2)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      LAT[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("LAT......: %s\n",LAT);  
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 3)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      LON[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("LON......: %s\n",LON);  
//------------------------------------------  
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 4)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      DEP[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("DEP......: %s\n",DEP);  
//------------------------------------------ 
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 5)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      AG[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("AG.......: %s  AG[0] = %c  AG[1] = %c AG[2] : %x\n",AG,AG[0],AG[1],AG[2]);
  
  switch(n)
  {
      case 0:
      AGENCY[0] = ' ';
      AGENCY[1] = ' ';
      AGENCY[2] = ' ';
      AGENCY[3] = '\0';
      break;
      case 1:
      AGENCY[0] = ' ';
      AGENCY[1] = ' ';
      AGENCY[2] = AG[0];
      AGENCY[3] = '\0';
      break;
      case 2:
      AGENCY[0] = ' ';
      AGENCY[1] = AG[0];
      AGENCY[2] = AG[1];
      AGENCY[3] = '\0';
      break;
      case 3:
      AGENCY[0] = AG[0];
      AGENCY[1] = AG[1];
      AGENCY[2] = AG[2];
      AGENCY[3] = '\0';
      break;
      case 4:
      AGENCY[0] = AG[0];
      AGENCY[1] = AG[1];
      AGENCY[2] = AG[3];
      AGENCY[3] = '\0';
      break;      
  }

//  printf("AGENCY: %s\n",AGENCY);
//  exit(0);
//------------------------------------------      
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 9)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      MTYP[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("MTYP.....: %s\n",MTYP);  
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 10)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      MAG[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("MAG......: %s\n",MAG);  
//------------------------------------------   
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 12)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
//  sprintf(WHERE," LOCALITY: ");
  for(k=j;k<600;k++)
  {
    if(line[k] != 0xa)
    {
//      printf("%c\n",line[k]);
      WHERE[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("WHERE....: %s\n",WHERE);  
//------------------------------------------   
}
int parse_line_isc(char line[])
{
  int k,j,n;
  int tel;


  for(k=0;k<25;k++)
  {
    ID[k]   = '\0'; 
    TIME[k] = '\0';
    LAT[k]  = '\0';
    LON[k]  = '\0';
    MTYP[k] = '\0';
    MAG[k]  = '\0';
    DEP[k]  = '\0';
    AG[k]   = '\0';
    AGENCY[k]  = '\0';
  }

  sscanf(line,"%s %s %s %s %s %s %s %s %s %s",DATE,TIME,dum,dum,LAT,LON,dum,dum,dum,DEP);
  sscanf(line,"%118c%s",dum,AG);
  sscanf(line,"%128c%s",dum,ID);
//  printf("%s  %s %s %s %s %s %s\n",DATE,TIME,LAT,LON,DEP,AG,ID);
    AGENCY[0]=AG[0];
    AGENCY[1]=AG[1];
    AGENCY[2]=AG[2];
    AGENCY[3] = '\0';



  
}
//------------------------------------------   

int les_linje2(FILE *streamfp,char linje[])
{
  int l;
  int br;
  br=0;
  for(l=0;l<600;l++)
  {
    linje[l]=fgetc(streamfp);
//    printf("%2d %2x %c\n",l,linje[l],linje[l]);
    if(linje[l] == 0xa)
    {
      br=2;
      break;
    }

    if(linje[l] == EOF)
    {
      printf("End of file.\n");

      br=1;
      return(br);
    }
  }
  return(br);
}

/***************************************************************************
 * read_parameter:
 *
 * Process the command line parameters.
 *
 * Returns 0 on success, and -1 on failure
 ***************************************************************************/
static int
read_parameter(int argcount, char **argvec)
{
  int optind;
  int error = 0;
  int len;
  int j;

  if (argcount <= 1)
  {
    help();
    return(-1);
  }
  sprintf(mindep,"0");                                  // default minimum depth
  sprintf(maxdep,"800");                                // default maximum depth
//printf("argcount: %d error: %d\n",argcount,error);
  /* Process all command line arguments */
  for (optind = 1; optind < argcount; optind++)
  {
    if (strcmp (argvec[optind], "-h") == 0)
    {
      help();
      exit (0);
    }
//    else if (strcmp (argvec[optind], "-fmt") == 0)         // input format
//    {
//      strcpy(command, argvec[++optind]);
//      sscanf(command,"%f",&val);
//      FORMAT = (int)val;
//    }
    else if (strcmp (argvec[optind], "-loop") == 0)         // input loop or one query
    {
      strcpy(command, argvec[++optind]);
      sscanf(command,"%d",&ival);
      loop_forever = ival;
    }      
    else if (strcmp (argvec[optind], "-tsrt") == 0)         // input tstart
    {
      strcpy(command, argvec[++optind]);
      sscanf(command,"%d",&ival);
      tstart = ival;
    }
    else if (strcmp (argvec[optind], "-tstp") == 0)         // input tstop
    {
      strcpy(command, argvec[++optind]);
      sscanf(command,"%d",&ival);
      tstop = ival;
    }
    else if (strcmp (argvec[optind], "-fm") == 0)          // SEISAN format
    {
      strcpy(command, argvec[++optind]);
      sscanf(command,"%d",&form);
    }      
    else if (strcmp (argvec[optind], "-ag") == 0)          // input agency
    {
      strcpy(command, argvec[++optind]);
      sscanf(command,"%d",&web_agency);
    }       
    else if (strcmp (argvec[optind], "-mag") == 0)         // input longitude
    {
      strcpy(command, argvec[++optind]);
      sprintf(minmag,"%s",command);
//      printf("%s\n",minmag);
    }
    else if (strcmp (argvec[optind], "-mind") == 0)         // input minimum depth
    {
      strcpy(command, argvec[++optind]);
      sprintf(mindep,"%s",command);
//      printf("%s\n",mindep);
    }    
    else if (strcmp (argvec[optind], "-maxd") == 0)         // input maximum depth
    {
      strcpy(command, argvec[++optind]);
      sprintf(maxdep,"%s",command);
//      printf("%s\n",maxdep);
    }       
    else if (strcmp (argvec[optind], "-srt") == 0)         // start-time
    {
      strcpy(command, argvec[++optind]);
      sprintf(starttime,"%s",command);
    }
    else if (strcmp (argvec[optind], "-end") == 0)         // end-time
    {
      strcpy(command, argvec[++optind]); 
      sprintf(endtime,"%s",command);
    }    
    else if (strcmp (argvec[optind], "-db") == 0)          // data base
    {
      strcpy(command, argvec[++optind]);
      len=strlen(command);
      sprintf(dbase,"%s",command);
      if(len > 5)
      {
        printf("Data-base name can not be more than 5 characters !\n");  
        exit(0);
      }else{
        for(j=0;j<5;j++)
          dbase[j]='_';
        for(j=0;j<len;j++)
          dbase[j]=command[j];
        dbase[5]='\0';
//        printf("Dbase: %s\n",dbase);
      }
    }    
 

      else
      {
          return -1;
	  fprintf(stderr, "Unknown option: %s\n", argvec[optind]);
	  exit (1);
      }
  }


  
  return 0;
}
//------------------------------------------------------------------------
// help
//------------------------------------------------------------------------
static void
help (void)
{
  fprintf(stderr,
    "Valid program options:\n"
    "-h                 show this help info\n"
    "-fm   int          SEISAN format: 1-old, 2-new (default: 1)\n"
    "-ag   int          Agency to query: 0-USGS,1-EMSC,2-ISC (default: 0)\n"
    "-loop int          1-loop forever, 0-one query (0=default)\n"
    "-tsrt int          minutes back, start-time\n"
    "-tstp int          minutes back, stop-time\n"
    "-mag  float        minimum magnitude\n"
    "-mind text         minimum depth\n"
    "-maxd text         maximum depth\n"    
    "-db   text         SEISAN database name, max 5 characters\n"
    "-srt  text         start-time, format: 2018-02-20T03:45:00\n"
    "-end  text         end-time, format  : 2018-02-22T08:23:00\n"
     "\n");
}
