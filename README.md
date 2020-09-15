# Final Project for Enterprise COBOL Course

## Creating the needed Datasets and uploading the files (uses ZOWE)

Replaces `HLQ` with your installation user

```
zowe files create pds 'HLQ.FINALS.COBOL'
zowe files create pds 'HLQ.FINALS.COPYLIB'
zowe files create pds 'HLQ.FINALS.JCL'
zowe files create pds 'HLQ.FINALS.LOAD' --rf U --bs 4096 --sz 2CYL --ss 2 --dst LIBRARY --db 5
zowe files create pds 'HLQ.FINALS.OBJS' --rl 80 --bs 6160 --sz 1CYL --db 10
zowe files create pds 'HLQ.FINALS.OUTPUT'
zowe files create ps 'HLQ.FINALS.PARTSUPP' --rl 473 --sz 1CYL --bs 4730
zowe files create ps 'HLQ.FINALS.STATEZIP' --rl 33 --bs 3300 --sz 1TRK
zowe files ul dtp CODE/ 'HLQ.FINALS.COBOL'
zowe files ul dtp copybooks/ 'HLQ.FINALS.COPYLIB'
zowe files ul ftds CODE/FINALEX.cbl 'HLQ.FINALS.COBOL(FINALEX)'
zowe files ul ftds DATA/zipcode.data 'HLQ.FINALS.STATEZIP'
zowe files ul ftds DATA/.data 'HLQ.FINALS.STATEZIP'
zowe files ul ftds DATA/sample.data 'Z80643.FINALS.PARTSUPP' 
```