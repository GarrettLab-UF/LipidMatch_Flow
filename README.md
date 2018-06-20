# LipidMatch_Flow
A user-friendly software covering the entire lipidomics data-processing workflow for LC-HRMS/MS data. Simply drag the correctly named vendor files onto the interface, select an output directory, and click Run. Behind the scenes, LipidMatch Flow incorporates MSConvert for file conversion, MZmine for peak picking optimized for each vendor, LipidMatch for identification, and combines adducts and polarities into a single file which can be uploaded into metaboanalyst for statistics. Feature finding uses unique algorithms along with MZmine to increase reproducibility of peak picking across samples, as was as increase data-processing speeds. Lipid identifications are obtained using the most comprehensive open-source in-silico MS/MS libraries to date. Currently works for Agilent Q-TOF, Thermo orbitrap, and SCIEX Q-TOF files. Future updates will include: normalization to lipid internal standards, acceptance of Bruker files, statistics, and pathway analysis. This is the first release, please report bugs, feedback, or just let me know the software worked well at: jeremykoelmel@gmail.com

For video tutorials visit: https://www.youtube.com/playlist?list=PLZtU6nmcTb5kAOHAPjtpWXwyjfpDnaB2M

Please read and agree to the following license for dependencies before downloading:

    MZmine: https://github.com/mzmine/mzmine2/blob/master/LICENSE.txt
    MSConvert: http://proteowizard.sourceforge.net/licenses.html

For software developers and to report bugs the GitHub page for LipidMatch is: https://github.com/GarrettLab-UF/LipidMatch

Jeremy P. Koelmel, Nicholas M. Kroeger, Candice Z. Ulmer, John A. Bowden, Rainey E. Patterson, Jason A. Cochran, Christopher W. W. Beecher, Timothy J. Garrett, Richard A. Yost: LipidMatch: an automated workflow for rule-based lipid identification using untargeted high-resolution tandem mass spectrometry data. BMC Bioinformatics. (2017) 18:331. doi: 10.1186/s12859-017-1744-3

Darren Kessner, Matt Chambers, Robert Burke, David Agus, and Parag Mallick:

ProteoWizard: Open Source Software for Rapid Proteomics Tools Development. Bioinformatics. (2008) 24(21): 2534–2536. doi: 10.1093/bioinformatics/btn323

Tomáš Pluskal, Sandra Castillo, Alejandro Villar-Briones, and Matej Oreši: MZmine 2: Modular Framework for Processing, Visualizing, and Analyzing Mass Spectrometry-Based Molecular Profile Data. BMC Bioinformatics (2010) 11(1): 395. doi: 10.1186/1471-2105-11-395
