var noerror = 0;
var object = {undefined : true};
if (object.undefined !== true) {
    noerror = 1;
}

//CHECK#2
var object = {undefined : true};
if (object["undefined"] !== true) {
    noerror = 2;
}

//CHECK#3
var object = {"true" : true};
if (object["true"] !== true) {
    noerror = 3;
}

//CHECK#4
var object = {"null" : true};
if (object["null"] !== true) {
    noerror = 4;
}
