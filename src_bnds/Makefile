ifdef ComSpec
    RM=del /F /Q
    COPY=copy
else
    RM=rm -rf
    COPY=cp
endif

all: fm 

fm: fm_bnds.tpl 
	@admb -f fm_bnds.tpl 


clean:
	@$(RM) fm_bnds.cpp
	@$(RM) fm_bnds.htp
	@$(RM) fm_bnds.obj

