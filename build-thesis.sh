
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

# copy output PDF to GoogleDrive for upload
function copyToGoogleDrive() {
cp thesis.pdf ~nikolas/GoogleDrive/PhD/
}

function GoogleDriveLinks() {
   echo ln -s ~nikolas/GoogleDrive/PhD/Thesis/figures  ~nikolas/Thesis/figures
   ln -s ~nikolas/GoogleDrive/PhD/Thesis/figures  ~nikolas/Thesis/figures
   for x in IL2RA IL2 KIR Appendix flowdatasets
    do
        echo ln -s ~nikolas/GoogleDrive/PhD/Thesis/$x/figures  ~nikolas/Thesis/$x/figures
        ln -s ~nikolas/GoogleDrive/PhD/Thesis/$x/figures  ~nikolas/Thesis/$x/figures
    done
}

GoogleDriveLinks
clean
build
copyToGoogleDrive


