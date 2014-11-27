while [ $# != 0 ]
do
	name=$1
	shift
	
	# echo ${name}| sed 's/\.md/\.htm/'
	html="`echo $name | sed 's/\.md/\.html/'`"
	# $html="echo ${name} | sed 's/\.md/\.htm/'"
	echo $html
	
	perl ../tools/Markdown_1.0.1/Markdown.pl --html4tags $name > $html  

done




