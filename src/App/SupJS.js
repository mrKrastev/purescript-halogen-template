
exports.cleanInputBox = () => {
        document.getElementById("inp").value = "";
        return 1;
    };
exports.disableInputBox = () => {
        document.getElementById("inp").disabled=true;
           return 1;
       };

 exports.changeParticleSpeed = (speed) => {
     if(speed==null){
        document.getElementById("speedInput").value=1;
        return 1;
     } 
        if(speed>99){
        speed=99;
        }
        document.getElementById("speedInput").value=Math.asin(speed*0.01)*(speed/2);
           return 1;
       };
exports.resizeMagic = (widthAmount) => {
        document.getElementById("magicWidthField").value=widthAmount+50;
           return 1;
       };

exports.renderMagicJs = () => {
        document.getElementById("particles-js").style.visibility="visible";
        return 1;
       };
exports.renderMagicJsPVP = () => {
        document.getElementById("particles-js").style.visibility="visible";
        document.getElementById("particles-jsp2").style.visibility="visible";
        return 1;
       };
exports.fixPVPmagicPositioning = () => {
    window.alert("aha");
        var magic1 =document.getElementById("particles-js");
        magic1.style.left="400px";
        magic1.style.top="300px";
        var magic2=document.getElementById("particles-jsp2");
        magic2.style.left="750px";
        magic2.style.top="300px";
        return 1;
       };
