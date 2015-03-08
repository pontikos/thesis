function compile() {
    pdflatex thesis
}

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
#rm thesis.ist
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

function copyPDFs() {
    #mv ~/thor/Thesis/figures/*.pdf ~/Thesis/figures/
    #cp ~/dil/Thesis/figures/*.pdf ~/PhD/Thesis/figures/
    #scp nikolas@stats0:~nikolas/Thesis/figures/*.pdf figures/
    echo rsync nikolas@stats0:~nikolas/Thesis/figures/*.pdf figures/
}

function chapter() {
pdflatex -jobname=thechap01 "\includeonly{chap01}\input{thesis}"
}

function makeindexes() {
makeindex -s cdots.ist genes
makeindex -s cdots.ist proteins
makeindex -s cdots.ist snps
makeindex -s cdots.ist contributors
}

function build() {
compile
makeglossaries thesis
compile
bibtex thesis
compile
if [[ "$?" == "1" ]]
then
    return
fi
compile
makeindexes
compile
makeglossaries thesis
compile
compile
compile
makeindexes
compile
compile
}

function GoogleDriveLinks() {
   echo ln -s ~/GoogleDrive/PhD/Thesis/figures  ~/PhD/Thesis/figures
   ln -s ~/GoogleDrive/PhD/Thesis/figures  ~/PhD/Thesis/figures
   for x in IL2RA IL2 KIR Appendix flowdatasets introduction2
    do
        ln -s ~/GoogleDrive/PhD/Thesis/$x/figures  ~/PhD/Thesis/$x/figures
    done
}

# copy output PDF to GoogleDrive for upload
function copyToGoogleDrive() {
cp thesis.pdf ~/GoogleDrive/PhD/Thesis/
}

# 
function gitPush() {
git commit -m 'thesis' *.tex */*.tex *.bib
git push
}



function all() {
#GoogleDriveLinks
clean
#copyPDFs
build
copyToGoogleDrive
gitPush
}

modes=$#
if [[ "$modes" == "" ]]
then
    echo all
    all
else
    while (( "$modes" ))
    do
        echo $1
        $1
        shift
        modes=$#
    done
fi


