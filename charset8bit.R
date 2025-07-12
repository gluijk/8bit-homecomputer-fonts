# Vintage 8-bit home computer fonts (ZX Spectrum, C64, Amstrad CPC)
# www.overfitting.net
# https://www.overfitting.net/

library(png)


# Read charsets positions and correspondences
lines=readLines("charsetsutf8.txt", warn=FALSE, encoding="UTF-8")
charsets_list=strsplit(lines, "\t")
header=charsets_list[[1]]  # use first row as header
data=charsets_list[-1]
df=as.data.frame(do.call(rbind, data), stringsAsFactors=FALSE)  # convert to df
df[, c(1, 3:5)]=lapply(df[, c(1, 3:5)], as.numeric)
colnames(df)=header
NCHAR=nrow(df)  # 96 different characters


# Read bitmaps forming each char on each charset -> alphabet
FONTSIZE=8  # 8x8 charset
charsets=c("zxspectrum", "c64", "amstradcpc")
NCHARSETS=length(charsets)

alphabet=array(0, dim=c(FONTSIZE, FONTSIZE, NCHAR, NCHARSETS))  # 4-D array
for (set in 1:NCHARSETS) {
    img=readPNG(paste0(charsets[set], "_charset.png"))
    NX=ncol(img)/FONTSIZE

    for (char in 1:NCHAR) {
        # Locate the requested character in img
        pos=df[char, set+2]
        col=((pos-1) %% NX) + 1
        row=ceiling(pos/NX)
        alphabet[,,char, set]=img[((row-1)*FONTSIZE+1):(row*FONTSIZE),
                                  ((col-1)*FONTSIZE+1):(col*FONTSIZE)]
    }
}


# Write all 3 alphabets in a single image
alphabetstrip=alphabet
dim(alphabetstrip)=c(FONTSIZE, FONTSIZE*NCHAR*NCHARSETS)  # rearrange as long strip

imgalphabet=matrix(0, nrow=FONTSIZE*NCHARSETS, ncol=FONTSIZE*NCHAR)  # 3 strips
for (set in 1:NCHARSETS) {
    imgalphabet[((set-1)*FONTSIZE+1):(set*FONTSIZE),]=
        alphabetstrip[1:FONTSIZE, ((set-1)*FONTSIZE*NCHAR+1):(set*FONTSIZE*NCHAR)]
}
writePNG(imgalphabet, "alphabets.png")


# Function returns colour array (image) with input text in desired charset (1-3)
# \n must be used to split lines in text
# All used characters must exist in the charset (no checking)
text2image=function(text, ncharset=2, FONTSIZE=8) {
    lines=strsplit(text, "\n")[[1]]  # split text by \n in a vector of strings
    NLINES=length(lines)
    MAXLEN=max(nchar(lines))
    
    img=matrix(1, nrow=NLINES*FONTSIZE, ncol=MAXLEN*FONTSIZE)
    for (i in 1:NLINES) {
        NCHARS=nchar(lines[i])
        if (NCHARS) {  # not empty line
            for (j in 1:NCHARS) {
                char=substring(lines[i], j, j)
                charnum=df$charnum[df$char==char]
                img[((i-1)*FONTSIZE+1):(i*FONTSIZE), ((j-1)*FONTSIZE+1):(j*FONTSIZE)]=
                    alphabet[1:FONTSIZE, 1:FONTSIZE, charnum, ncharset]
            }
        }
    }
    img=replicate(3, img)
    
    # Colours
    if (ncharset==1) {  # ZX Spectrum
        ink       =c(0,0,0)       / 255
        background=c(191,191,191) / 255
    } else if (ncharset==2) {  # C64
        ink       =c(108,94,181)  / 255
        background=c(53,40,121)   / 255        
    } else if (ncharset==3) {  # Amstrad CPC
        ink       =c(255,255,0)   / 255
        background=c(0,0,128)     / 255        
    } else return(img)  # B&W output image
    
    imgout=img
    for (chan in 1:3) {
        imgout[,,chan]=background[chan]        
        imgout[,,chan][img[,,chan]==0]=ink[chan]
    }

    return(imgout)
}


#######################################

# Examples
text=paste0(
    '"The Raven" by Edgar Allan Poe (January 29, 1845)\n\n',
    'Once upon a midnight dreary, while I pondered, weak and weary,\n',
    'Over many a quaint and curious volume of forgotten lore,\n',
    'While I nodded, nearly napping, suddenly there came a tapping,\n',
    'As of some one gently rapping, rapping at my chamber door...'
)

text=paste0("The quick brown fox jumps over the lazy dog\n",
            "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG\n",
            "01234567890 +-*/ ?!\"' ()[]<>{} .,;:= \\|@$£%&#~")

text=paste0(
    '"Hotel California" by The Eagles (December 8, 1976)\n\n',
    '(...)\nMirrors on the ceiling, the pink champagne on ice\n',
    'And she said, "We are all just prisoners here of our own device"\n',
    'And in the master\'s chambers, they gathered for the feast\n',
    'They stab it with their steely knives, but they just can\'t kill the beast\n\n',
    'Last thing I remember, I was running for the door\n',
    'I had to find the passage back to the place I was before\n',
    '"Relax" said the night man, "We are programmed to receive\n',
    'You can check out any time you like, but you can never leave"...'
)

for (set in 1:NCHARSETS) {
    img=text2image(text, set)
    writePNG(img, paste0("example_", charsets[set], ".png")) 
}

