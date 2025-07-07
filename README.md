# Grapheme to phoneme conversion of West Frisian text

With this software West Frisian text can be converted into phonetic IPA transcriptions.

## Running the command line script

The command line script `graph2phon.R` requires the models `g2p.fst`, `g2p_stress.fst` and `frisian_frysk-ud-1.00-240313.udpipe` as well. These models should reside in the same directory as where this script is stored.

The file `g2p.fst` contains a trained g2p model that was trained on the basis of the Frysk Hânwurdboek, the Foarkarswurdlist and a short supplementary list. It does not include primary stress marks. The file `g2p_stress.fst` contains a trained g2p model that was trained on the basis of the Frysk Hânwurdboek and a short supplementary list. It includes primary stress marks. The file `fy_frysk-ud-1.0-20240313.udpipe` contains a trained udpipe model.

The file `Frysk.txt` is added as an example text and can be processed by entering the following command in a terminal:

`./graph2phon.R -i Frysk.txt -f -o Frysk.xlsx -x`<br>

where the result is stored as an Excel spreadsheet in Frysk.xlsx,<br>
or read from pipe:

`cat Frysk.txt|./graph2phon.R -i - -e -t > Frysk.tsv`<br>

or read from user input (close with Ctrl-d):

`./graph2phon.R -i - -e -t > Frysk.tsv`<br>

where the result is stored as a tab-separated file in `Frysk.tsv`.

For information about usage and options enter:<br>

`./graph2phon.R -h`

### Linux (Debian / Ubuntu) installs

Before running the app or the script for the first time, enter the following commands in a terminal:<br>

`sudo pip3 install phonetisaurus`<br>
`sudo pip3 install lingpy`<br>
`ln -s /usr/bin/python3.x /usr/bin/python3`<br>
`sudo apt install r-base-core`<br>
`sudo apt install libcurl4-openssl-dev`<br>
`sudo apt install libxml2`<br>
`sudo apt install libxml2-dev`<br>
`sudo apt install libssl-dev`<br>
`sudo apt install libpoppler-cpp-dev`<br>
`sudo apt install libjpeg-dev`<br><br>

With `python3.x` is meant version 3.8 or higher, where `x` should be replaced by the minor version (or subversion) number.

## Contact

E-mail: `wheeringa [at] fryske-akademy [dot] nl`
