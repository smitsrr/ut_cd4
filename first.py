import xml.etree.ElementTree as ET
from xml.etree import ElementTree
tree = ET.parse('detail.xml')
root = tree.getroot()

# Shows all of the overview data!!
#for child in root:
#    print( child.tag, child.attrib)

## https://docs.python.org/2/library/xml.etree.elementtree.html

#for contest in root.iter('Contest'):
#    print(contest.attrib)
# prints the attributes of the head node

house_race = tree.findall('.//Contest[@text="U.S. REPRESENTATIVE DISTRICT #4"]')

# 'year' nodes that are children of nodes with name='Singapore'
house_race = tree.findall(".//*[@text='U.S. REPRESENTATIVE DISTRICT #4']/Choice")
xmlstr = ElementTree.tostring(house_race).decode()
print(xmlstr)


##
##for contest in root.iter('Contest'):
##    print(Choice.attrib)
##    print(Choice.find('*/Choice'))



