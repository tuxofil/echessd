// File    : echessd.js
// Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
// Created : 22 Feb 2012
// License : FreeBSD
// Description : mouse support functions

function clr(){
    document.getElementById('edmv').value = '';
    var i;
    for(i = 0; i < ACs.length; i++)
        document.getElementById(ACs[i]).className = (indexOf(ACs[i], LPs))?'lastply':'cell';
    }

function mv(crd){
    var mvField = document.getElementById('edmv');
    var i1index = indexOf(crd, I1s);
    if(i1index){
        clr();
        mvField.value += crd;
        document.getElementById(crd).className = 'ply';
        if(I2s[i1index - 1].length == 1){
            mv(I2s[i1index - 1][0]);
        }else{
            var i;
            for(i = 0; i < I2s[i1index - 1].length; i++)
                document.getElementById(I2s[i1index - 1][i]).className = 'hint';
        }
    }else{
        var ply = mvField.value;
        if(ply.length >= 4 && ply.substr(2, 2) == crd) return 1;
        i1index = indexOf(ply, I1s);
        if(i1index > 0){
            if(indexOf(crd, I2s[i1index - 1])){
                mvField.value += crd;
                document.getElementById(crd).className = 'ply';
            }
        }else clr();
    }
}

function indexOf(item, arr){
    var i;
    for(i = 0; i < arr.length; i++)
        if(arr[i] == item) return i + 1;
    return 0;
}

