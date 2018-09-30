cower -d dmenu2
cd dmenu2
makepkg -ci
cd ..
rm -r dmenu2

cower -d polybar
cd polybar
makepkg -ci
cd ..
rm -rf polybar

cower -d zscroll-git
cd zscroll-git
makepkg -ci
cd ..
rm -rf zscroll-git

cower -d siji-git
cd siji-git
makepkg -ci
cd ..
rm -rf siji-git

cower -d dropbox
cd dropbox
makepkg -ci
cd ..
rm -rf dropbox
