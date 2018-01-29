#Copyright (c) 2015, Mohit Makkar
#All rights reserved.

#Redistribution and use in source and binary forms, with or without
#modification, are permitted provided that the following conditions are met:
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#    * The name of the copyright holders may not be used to endorse or promote 
#      products derived from this software without specific prior written permission.

#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
#ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
#WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
#DISCLAIMED. IN NO EVENT SHALL Mohit Makkar BE LIABLE FOR ANY
#DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
#(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
#LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
#ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


#The references are:
# * http://tartarus.org/martin/PorterStemmer/def.txt
# * http://tartarus.org/martin/PorterStemmer/voc.txt
# * http://tartarus.org/martin/PorterStemmer/output.txt



#Porter's Algorithm in R
#first two functions are not a part of Porter's Algorithm but act as tools for defining other functions
#the functions str_detect and str_replace are inbuilt functions from library(sringr)
#please use small letters as input
#Run the code and then call the stem function to stem any word, for example stem("drawing")


library(stringr)
#the following function splits a word into characters
#for instance, for the input "hello", it returns "h" "e" "l" "l" "o" 
splitstring<-function(x){
strsplit(x, NULL)[[1]]
}


#function that returns a logical vector of TRUE/FALSE depending on vowel/consonant 
#for instance, for the input "cry", it returns "FALSE" "FALSE" "TRUE"
h1<-function(x)
{
l<-str_detect("aeiou", splitstring(x))
k<-length(splitstring(x))
z<-splitstring(x)[1:k]
i<-2;
	while(k>1&&i<=k){
		if(splitstring(x)[i]=="y"){
			if(!str_detect(z[i-1], "[aeiou]")){
				l[i]<-TRUE
			}
		}
	i=i+1;
	}
l
}


#function for returning the interger value of m 
m<-function(x1,y1="")
{ 
k<-(length(splitstring(x1))-length(splitstring(y1)))
x<-paste0(splitstring(x1)[1:k],collapse="")
count=0;

y<-h1(x);

for(i in 1:length(y)){	
	if((i+1)<=length(y)){	
		if(y[i]){
			if(!y[i+1]){
				count=count+1;		
			}
		}
	}
}
count
}

#function returns TRUE if and only if there is a vowel in the word left after you remove suffix y1 from the word x1
v<-function(x1,y1="")
	{ 
	check<-FALSE
	k<-(length(splitstring(x1))-length(splitstring(y1)))
	w<-paste0(splitstring(x1)[1:k],collapse="")
	z<-splitstring(w)[1:k]
	i<-2;
	while(k>1&&i<=k){
				if(z[i]=="y")
				{
					if(!str_detect(z[i-1], "[aeiou]"))
					{
					check<-TRUE
					}	
				}
			i=i+1;
			}

	if(str_detect(w, "[aeiou]"))
	check<-TRUE
	check
}



#function returns TRUE if and only if there is a double consonant at the end of the word left after you remove suffix y1 from the word x1
d<-function(x1,y1="")
	{ check<-FALSE

	k<-(length(splitstring(x1))-length(splitstring(y1)))
	if(k>1)
	{
	if(splitstring(x1)[k]==splitstring(x1)[k-1] && !v(splitstring(x1)[k]) )
	check<-TRUE
	}
	check
	}

#function returns TRUE if and only if there is a sequence of cvc at the end of the word left after you remove suffix y1 from the word x1
#except when the second consonant is w,x or y
cvc<-function(x1,y1="")
{ 
check<-FALSE; 
k<-(length(splitstring(x1))-length(splitstring(y1)))
if(k>=3)
{
c2<-splitstring(x1)[k]
v<-splitstring(x1)[k-1]
c1<-splitstring(x1)[k-2]

t1<-str_detect(c1, "[aeiou]")
t2<-str_detect(v, "[aeiouy]")
t3<-str_detect(c2, "[aeiou]")
t4<-str_detect(c2, "[wxy]")
if(!t1&&t2&&!t3&&!t4)
check<-TRUE
}
check
}

#actual function that stems the word x
stem<-function(x){ if(length(splitstring(x))>2){
	if(str_detect(x, "s$")){
		if(str_detect(x, "sses$")){
		x<-str_replace(x,"sses$","ss")

		} else if(str_detect(x, "ies$")){
		x<-str_replace(x,"ies$","i")

		} else if(str_detect(x, "ss$")){
		x<-str_replace(x,"ss$","ss")

		} else if(str_detect(x, "s$")){
		x<-str_replace(x,"s$","")
		}
	}

#Step 1b)
flag<-0;
if(str_detect(x, "eed$")){
	if(m(x,"eed")>0)
	x<-str_replace(x,"eed$","ee")
	
} else if(str_detect(x, "ed$")){
	if(v(x,"ed")){
	x<-str_replace(x,"ed$","")
	flag<-1;
	}
} else if(str_detect(x, "ing$")){
	if(v(x,"ing")){
	x<-str_replace(x,"ing$","")
	flag<-1;
	}
}

if(flag==1){
	if(str_detect(x, "at$")){
	x<-str_replace(x,"at$","ate")
	
	} else if(str_detect(x, "bl$")){
        x<-str_replace(x,"bl$","ble")

	} else if(str_detect(x, "iz$")){
	  x<-str_replace(x,"iz$","ize")

	} else if(!str_detect(x, "[lsz]$")&&d(x)){
	  x<-paste0(splitstring(x)[1:(length(splitstring(x))-1)],collapse="")
	
	} else if(m(x)==1&&cvc(x)){
	  x<-paste0(x,"e",collapse="")
	}	
}

#Step 1c)
if(str_detect(x, "y$")){
	if(v(x,"y"))
	x<-str_replace(x,"y$","i")
}	


#Step 2
if(str_detect(x, "ational$")){
	if(m(x,"ational")>0)
	x<-str_replace(x,"ational$","ate")

} else if(str_detect(x, "tional$")){
	if(m(x,"tional")>0)
	x<-str_replace(x,"tional$","tion")

} else if(str_detect(x, "enci$")){
	if(m(x,"enci")>0)
	x<-str_replace(x,"enci$","ence")

} else if(str_detect(x, "anci$")){
	if(m(x,"anci")>0)
	x<-str_replace(x,"anci$","ance")

} else if(str_detect(x, "izer$")){
	if(m(x,"izer")>0)
	x<-str_replace(x,"izer$","ize")

} else if(str_detect(x, "logi$")){
	if(m(x,"logi")>0)
	x<-str_replace(x,"logi$","log")

} else if(str_detect(x, "bli$")){
	if(m(x,"bli")>0)
	x<-str_replace(x,"bli$","ble")

} else if(str_detect(x, "alli$")){
	if(m(x,"alli")>0)
	x<-str_replace(x,"alli$","al")

} else if(str_detect(x, "entli$")){
	if(m(x,"entli")>0)
	x<-str_replace(x,"entli$","ent")

} else if(str_detect(x, "eli$")){
	if(m(x,"eli")>0)
	x<-str_replace(x,"eli$","e")

} else if(str_detect(x, "ousli$")){
	if(m(x,"ousli")>0)
	x<-str_replace(x,"ousli$","ous")

} else if(str_detect(x, "ization$")){
	if(m(x,"ization")>0)
	x<-str_replace(x,"ization$","ize")

} else if(str_detect(x, "ation$")){
	if(m(x,"ation")>0)
	x<-str_replace(x,"ation$","ate")

} else if(str_detect(x, "ator$")){
	if(m(x,"ator")>0)
	x<-str_replace(x,"ator$","ate")

} else if(str_detect(x, "alism$")){
	if(m(x,"alism")>0)
	x<-str_replace(x,"alism$","al")

} else if(str_detect(x, "iveness$")){
	if(m(x,"iveness")>0)
	x<-str_replace(x,"iveness$","ive")

} else if(str_detect(x, "fulness$")){
	if(m(x,"fulness")>0)
	x<-str_replace(x,"fulness$","ful")

} else if(str_detect(x, "ousness$")){
	if(m(x,"ousness")>0)
	x<-str_replace(x,"ousness$","ous")

} else if(str_detect(x, "aliti$")){
	if(m(x,"aliti")>0)
	x<-str_replace(x,"aliti$","al")

} else if(str_detect(x, "iviti$")){
	if(m(x,"iviti")>0)
	x<-str_replace(x,"iviti$","ive")

} else if(str_detect(x, "biliti$")){
	if(m(x,"biliti")>0)
	x<-str_replace(x,"biliti$","ble")
}
#Step 3

if(str_detect(x, "icate$")){
	if(m(x,"icate")>0)
	x<-str_replace(x,"icate$","ic")

} else if(str_detect(x, "ative$")){
	if(m(x,"ative")>0)
	x<-str_replace(x,"ative$","")

} else if(str_detect(x, "alize$")){
	if(m(x,"alize")>0)
	x<-str_replace(x,"alize$","al")

} else if(str_detect(x, "iciti$")){
	if(m(x,"iciti")>0)
	x<-str_replace(x,"iciti$","ic")

} else if(str_detect(x, "ical$")){
	if(m(x,"ical")>0)
	x<-str_replace(x,"ical$","ic")

} else if(str_detect(x, "ful$")){
	if(m(x,"ful")>0)
	x<-str_replace(x,"ful$","")

} else if(str_detect(x, "ness$")){
	if(m(x,"ness")>0)
	x<-str_replace(x,"ness$","")
}

#Step 4
if(str_detect(x, "al$")){
	if(m(x,"al")>1)
	x<-str_replace(x,"al$","")

} else if(str_detect(x, "ance$")){
	if(m(x,"ance")>1)
	x<-str_replace(x,"ance$","")

} else if(str_detect(x, "ence$")){
	if(m(x,"ence")>1)
	x<-str_replace(x,"ence$","")

} else if(str_detect(x, "er$")){
	if(m(x,"er")>1)
	x<-str_replace(x,"er$","")

} else if(str_detect(x, "ic$")){
	if(m(x,"ic")>1)
	x<-str_replace(x,"ic$","")

} else if(str_detect(x, "able$")){
	if(m(x,"able")>1)
	x<-str_replace(x,"able$","")

} else if(str_detect(x, "ible$")){
	if(m(x,"ible")>1)
	x<-str_replace(x,"ible$","")

} else if(str_detect(x, "ant$")){
	if(m(x,"ant")>1)
	x<-str_replace(x,"ant$","")

} else if(str_detect(x, "ement$")){
	if(m(x,"ement")>1)
	x<-str_replace(x,"ement$","")

} else if(str_detect(x, "ment$")){
	if(m(x,"ment")>1)
	x<-str_replace(x,"ment$","")

} else if(str_detect(x, "ent$")){
	if(m(x,"ent")>1)
	x<-str_replace(x,"ent$","")

} else if(str_detect(x, "sion$")||str_detect(x, "tion$")){ 
	if(m(x,"ion")>1)
	x<-str_replace(x,"ion$","")

} else if(str_detect(x, "ou$")){
	if(m(x,"ou")>1)
	x<-str_replace(x,"ou$","")

} else if(str_detect(x, "ism$")){
	if(m(x,"ism")>1)
	x<-str_replace(x,"ism$","")

} else if(str_detect(x, "ate$")){
	if(m(x,"ate")>1)
	x<-str_replace(x,"ate$","")

} else if(str_detect(x, "iti$")){
	if(m(x,"iti")>1)
	x<-str_replace(x,"iti$","")

} else if(str_detect(x, "ous$")){
	if(m(x,"ous")>1)
	x<-str_replace(x,"ous$","")

} else if(str_detect(x, "ive$")){
	if(m(x,"ive")>1)
	x<-str_replace(x,"ive$","")

} else if(str_detect(x, "ize$")){
	if(m(x,"ize")>1)
	x<-str_replace(x,"ize$","")
}

#Step 5a)

if(str_detect(x, "e$")){
	if(m(x,"e")>1){
	x<-str_replace(x,"e$","")
	} else if(m(x,"e")==1&& !cvc(x,"e")){
	x<-str_replace(x,"e$","")
	}
}
#Step 5b)
if(m(x)>1&&str_detect(x, "ll$")){
x<-str_replace(x,"l$","")
}
}
return(x)
}