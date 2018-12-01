So stoked for this analysis! 

Finally got a shape file with the same precinct IDs as the county election results file (for UT county at least). 

Need to request the damn pdf file from Utah county in basically anything other than pdf. 

Step #1 - download data from the county clerk's website. 
-- Salt Lake county at least offered xml format. 
-- Utah county by default offers a pdf. need to request a better format from their office. 

Ste #2 -Parse XML
[images/attribute_values.jpg]
It took me an embarrasing amount of time to get from the xml to a place where I could extract the data elements I need. That's party because they created the xml by storing the data as values of attributes instead of xml values. 

Every tutorial I saw that used `xmlValue` to get the values from the nodes didn't work because the values are actually stored in attribute values. So I had to use `xmlGetAttr()` instead. 
