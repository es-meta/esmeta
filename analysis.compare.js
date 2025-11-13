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
        if (name === 'eval')  { return {result: val}; }
        console.log('read:', name);
        return {result: val};
    },

    /**
     * This callback is called after a property of an object is accessed.
     *
     * @param {number} iid - Static unique instruction identifier of this callback
     * @param {*} base - Base object
     * @param {string|*} offset - Property
     * @param {*} val - Value of <code>base[offset]</code>
     * @param {boolean} isComputed - True if property is accessed using square brackets.  For example,
     * <tt>isComputed</tt> is <tt>true</tt> if the get field operation is <tt>o[p]</tt>, and <tt>false</tt>
     * if the get field operation is <tt>o.p</tt>
     * @param {boolean} isOpAssign - True if the operation is of the form <code>o.p op= e</code>
     * @param {boolean} isMethodCall - True if the get field operation is part of a method call (e.g. <tt>o.p()</tt>)
     * @returns {{result: *} | undefined} - If an object is returned, the value of the get field operation  is
     * replaced with the value stored in the <tt>result</tt> property of the object.
     */
        getField: function (iid, base, offset, val, isComputed, isOpAssign, isMethodCall) {
        // const offSetStr = (typeof offset === 'string' || typeof offset === 'symbol') ? offset : (console.log("WTF"), offset.toString());
        // console.log('getField:', offSetStr, typeof offset, iid);
        return {result: val};
        },

        putField: function (iid, base, offset, val, isComputed, isOpAssign) {
            // const offSetStr = (typeof offset === 'string' || typeof offset === 'symbol') ? offset : (console.log("WTF"), offset.toString());
            // console.log('putField:', offSetStr, typeof offset, iid);
            return {result: val};
        },
    
    /**
         * This callback is called before the execution of a function body starts.
         *
         * @param {number} iid - Static unique instruction identifier of this callback
         * @param {function} f - The function object whose body is about to get executed
         * @param {*} dis - The value of the <tt>this</tt> variable in the function body
         * @param {Array} args - List of the arguments with which the function is called
         * @returns {undefined} - Any return value is ignored
         */
        // functionEnter: function (iid, f, dis, args) {
        //     console.log('functionEnter:', '...');
        // },
};
}());

