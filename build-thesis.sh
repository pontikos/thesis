
function clean() {
#annoying files created by GoogleDrive
find . -type f -name "*~" -exec rm -f {} \;
#find . -type f -name "* (1).tex" -exec rm -f {} \;
find . -type f -name "*\[Conflict\].tex" -exec rm -f {} \;
#annoying file created by Finder
find . -type f -name "Icon?" -exec rm -f {} \;
rm thesis.acn
rm thesis.acr
rm thesis.alg
rm thesis.aux
rm thesis.bbl
rm thesis.blg
rm thesis.brf
rm thesis.glg
rm thesis.glo
rm thesis.gls
rm thesis.ist
rm thesis.lof
rm thesis.log
rm thesis.lot
rm thesis.out
rm thesis.toc
rm *.lof
rm *.toc 
rm *.ind 
rm *.idx 
rm *.ilg 
rm *.ist 
rm *.aux 
rm *.log 
rm *.nlo 
rm *.out
rm *.log
rm *.lot
rm *.blg *.brf 
rm *.bbl
rm *.glo
rm *.cb2
rm *.cb
rm *.acn *.alg *.acr
}

function chapter() {
pdflatex -jobname=thechap01 "\includeonly{chap01}\input{thesis}"
}

function makeindexes() {
makeindex genes
makeindex proteins
makeindex snps
makeindex contributors
}

function build() {
pdflatex thesis
makeglossaries thesis
pdflatex thesis
bibtex thesis
pdflatex thesis
if [[ "$?" == "1" ]]
then
    return
fi
pdflatex thesis
makeindexes
pdflatex thesis
makeglossaries thesis
pdflatex thesis
pdflatex thesis
pdflatex thesis
makeindexes
pdflatex thesis
pdflatex thesis
}

function GoogleDriveLinks() {
   echo ln -s ~/GoogleDrive/PhD/Thesis/figures  ~/Thesis/figures
   ln -s ~/GoogleDrive/PhD/Thesis/figures  ~/Thesis/figures
   for x in IL2RA IL2 KIR Appendix flowdatasets introduction2
    do
        echo ln -s ~/GoogleDrive/PhD/Thesis/$x/figures  ~/Thesis/$x/figures
        ln -s ~/GoogleDrive/PhD/Thesis/$x/figures  ~/Thesis/$x/figures
    done
}

# copy output PDF to GoogleDrive for upload
function copyToGoogleDrive() {
cp thesis.pdf ~nikolas/GoogleDrive/PhD/Thesis/
}



GoogleDriveLinks
clean
build
copyToGoogleDrive


