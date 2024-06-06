final: prev:
with prev;
{
  myTexlive = with final.texlive; (
    combine {
      inherit (final.texlive)
        academicons
        arydshln
        biber
        biblatex
        cm-unicode
        enumitem
        fontawesome5
        latexmk
        moderncv
        moderntimeline
        multirow
        scheme-medium
        ;
    }
  );
  myFontsConf = final.makeFontsConf {
    fontDirectories = [
      "${final.myTexlive}/share/texmf/"
    ];
  };
  thesis = stdenv.mkDerivation {
    name = "thesis.pdf";
    buildInputs = [ final.myTexlive ];
    src = ./.;
    FONTCONFIG_FILE = final.myFontsConf;
    buildPhase = ''
      (latexmk -xelatex main.tex |& uniq) || (echo "[E] .log file:" ; uniq < main.log >&2 ; exit 1)
    '';
    installPhase = ''
      install -m444 thesis.pdf $out
    '';
  };
}
