# Vintage 8-bit home computer fonts (ZX Spectrum, Commodore 64, Amstrad CPC, MSX)
# www.overfitting.net
# https://www.overfitting.net/2025/07/fuentes-de-los-ordenadores-de-8-bits-zx.html

library(png)


# Read charsets positions and correspondences
lines=readLines("charsetsutf8.txt", warn=FALSE, encoding="UTF-8")
charsets_list=strsplit(lines, "\t")
header=charsets_list[[1]]  # use first row as header
data=charsets_list[-1]
df=as.data.frame(do.call(rbind, data), stringsAsFactors=FALSE)
colnames(df)=header
df[, c(1, 3:6)]=lapply(df[, c(1, 3:6)], as.numeric)
NCHAR=nrow(df)  # 96 different characters


# Read bitmaps forming each char on each charset -> alphabet
FONTSIZE=8  # 8x8 charset
charsets=c("zxspectrum", "commodore64", "amstradcpc", "msx")
NCHARSET=length(charsets)  # 4 charsets

alphabet=array(0, dim=c(FONTSIZE, FONTSIZE, NCHAR, NCHARSET))  # 4-D array
for (set in 1:NCHARSET) {
    img=readPNG(paste0("charset_", charsets[set], ".png"))
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


# Write all 4 alphabets in a single image
alphabetstrip=alphabet
dim(alphabetstrip)=c(FONTSIZE, FONTSIZE*NCHAR*NCHARSET)  # rearrange as long strip

imgalphabet=matrix(0, nrow=FONTSIZE*NCHARSET, ncol=FONTSIZE*NCHAR)  # now 4 strips
for (set in 1:NCHARSET) {
    imgalphabet[((set-1)*FONTSIZE+1):(set*FONTSIZE),]=
        alphabetstrip[1:FONTSIZE, ((set-1)*FONTSIZE*NCHAR+1):(set*FONTSIZE*NCHAR)]
}
writePNG(imgalphabet, "alphabets.png")


# Function that returns colour image with input text in desired charset (1-4)
# \n must be used to split lines in text
# If character doesn't exist in the charset it is replaced by a space
text2image=function(text, ncharset=2) {
    FONTSIZE=8
    
    # Charset standard colours
    if (ncharset==1) {  # ZX Spectrum
        ink       =c(0,0,0)       / 255
        background=c(192,192,192) / 255  # #C0C0C0 normal white
    } else if (ncharset==2) {  # Commodore 64
        ink       =c(134,122,222) / 255  # #867ADE
        background=c(72,58,170)   / 255  # #483AAA    
    } else if (ncharset==3) {  # Amstrad CPC
        ink       =c(255,255,0)   / 255
        background=c(0,0,128)     / 255        
    } else if (ncharset==4) {  # MSX
        ink       =c(255,255,255) / 255
        background=c(32,32,255)   / 255  
    } else return(-1)
    
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
                # If char doesn't exist in charset is replaced by empty space
                if (!length(charnum)) charnum=df$charnum[df$char==' ']
                img[((i-1)*FONTSIZE+1):(i*FONTSIZE), ((j-1)*FONTSIZE+1):(j*FONTSIZE)]=
                    alphabet[1:FONTSIZE, 1:FONTSIZE, charnum, ncharset]
            }
        }
    }

    imgout=replicate(3, img)  # colour array
    for (chan in 1:3) {
        imgout[,,chan][img==0]=ink[chan]
        imgout[,,chan][img==1]=background[chan]        
    }
    
    return(imgout)
}


#######################################

# Examples

text="hello, world"

text=paste0(
    ' !"#$%&\'()*+,-./\n',
    '0123456789:;<=>?\n',
    '@ABCDEFGHIJKLMNO\n',
    'PQRSTUVWXYZ[\\]↑_\n',
    '£abcdefghijklmno\n',
    'pqrstuvwxyz{|}~©'
)

text=paste0(
    '"The Raven" by Edgar Allan Poe (January 29, 1845)\n\n',
    'Once upon a midnight dreary, while I pondered, weak and weary,\n',
    'Over many a quaint and curious volume of forgotten lore,\n',
    'While I nodded, nearly napping, suddenly there came a tapping,\n',
    'As of some one gently rapping, rapping at my chamber door...'
)

text=paste0("The quick brown fox jumps over the lazy dog\n",
            "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG\n",
            "01234567890 +-*/ ?!\"' ()[]<>{} .,;:= \\|_@$£%&#~")

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

for (set in 1:NCHARSET) {
    img=text2image(text, set)
    writePNG(img, paste0("example_", charsets[set], ".png")) 
}
