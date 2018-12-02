import xml.etree.ElementTree as ET
tree = ET.parse('detail.xml')
root = tree.getroot()

# Shows all of the overview data!!
#for child in root:
#    print( child.tag, child.attrib)

## https://docs.python.org/2/library/xml.etree.elementtree.html

for contest in root.iter('Contest'):
    print(contest.attrib)

#                        >>> for neighbor in root.iter('neighbor'):
#...     print neighbor.attrib

