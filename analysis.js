(function(){
  J$.analysis = {
     /**
       * This callback is called after a variable is read.
       *
       * @param {number} iid - Static unique instruction identifier of this callback
       * @param {string} name - Name of the variable being read
       * @param {*} val - Value read from the variable
       * @param {boolean} isGlobal - True if the variable is not declared using <tt>var</tt> (e.g. <tt>console</tt>)
       * @param {boolean} isScriptLocal - True if the variable is declared in the global scope using <tt>var</tt>
       * @returns {{result: *} | undefined} - If an object is returned, the result of the read operation is
       * replaced with the value stored in the <tt>result</tt> property of the object.
       */
    read: function (iid, name, val, isGlobal, isScriptLocal) {
        console.log(name);
        return {result: val};
    }
};
}());

