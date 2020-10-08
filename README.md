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
zowe files create ps 'HLQ.FINALS.PARTS' --rl 72 --bs 720 
zowe files create ps 'HLQ.FINALS.ADDRS' --rl 68 --bs 1360 
zowe files create ps 'HLQ.FINALS.PURCHASE'  --rl 34 --bs 3400
zowe files create ps 'HLQ.FINALS.GOODDATA' --rl 473 --sz 1CYL --bs 4730
zowe files ul dtp copybooks/ 'HLQ.FINALS.COPYLIB'
zowe files ul ftds CODE/FINALEX.cbl 'HLQ.FINALS.COBOL(FINALEX)'
zowe files ul ftds DATA/zipcode.data 'HLQ.FINALS.STATEZIP'
zowe files ul ftds DATA/.data 'HLQ.FINALS.STATEZIP'
zowe files ul ftds DATA/sample.data 'HLQ.FINALS.PARTSUPP' 
```

# How to Build and Run

1. Build the programs (can use the `COMPLINK` JCL provided for this)
   1. Subprograms first
      1. `ADDREDIT`
      2. `POEDIT`
      3. `SUPPEDIT`
      4. `PARTEDIT`
   2. `FINALEX`
   3. `FINALRPT`
2. Use `COBGO` to run all the needed steps
   1. If you can't
      1. Run First `FINALEX`, saving the output on GOODDATA dataset (LRECL 473)
      2. `SORT` the GOODDATA dataset and save it on another dataset (LRECL 473)
      3. Use the dataset from step 2 as input (DD GOODDATA) for `FINALRPT`

# Subprograms Authors

| Subprograms |  Author |
|-:|:-:|   
| Suppliers(SUPPEDIT ) | Guillermo |
| SupAdress (ADREDIT  ) | Fabio | 
| Purchase (POEDIT)  | Judy |
| Part (PARTEDIT) | Ivanna |