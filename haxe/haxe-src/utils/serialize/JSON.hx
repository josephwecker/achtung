/* JSON serializer and deserializer, with special sauce.  Also condensed into a
 * single file for now.
 *
 * (Heavily) derived from:
 *   hxjson2 (http://lib.haxe.org/p/hxJson2)
 *   by Philipp Klose (philipp.klose@byteanvil.com)
 * (which was) Ported from:
 *   as3corelib (http://code.google.com/p/as3corelib/)
 *   (com.adobe.serialization.JSON)
 */

package utils.serialize;

#if neko
import neko.Utf8;
#elseif php
import php.Utf8;
#end

/**
 * Primary entry-point with static functions encode and decode.
 */
class JSON {

    /**
     * Encodes a object into a JSON string.
     *
     * @param o The object to create a JSON string for
     * @return the JSON string representing o
     */
    public static function encode(o:Dynamic):String {
        return new JSONEncoder(o).getString();
    }

    /**
     * Decodes a JSON string into a native object.
     *
     * @param s The JSON string representing the object
     * @return A native object as specified by s
     */
    public static function decode(s:String,strict:Bool=true):Dynamic {
        return new JSONDecoder(s,strict).getValue();
    }

}

private enum JSONTokenType {
    UNKNOWN;
    COMMA;
    LEFT_BRACE;
    RIGHT_BRACE;
    LEFT_BRACKET;
    RIGHT_BRACKET;
    COLON;
    TRUE;
    FALSE;
    NULL;
    STRING;
    NUMBER;
    NAN;
}

private class JSONDecoder {
    var strict:Bool;
    /** The value that will get parsed from the JSON string */
    var value:Dynamic;
    /** The tokenizer designated to read the JSON string */
    var tokenizer:JSONTokenizer;
    /** The current token from the tokenizer */
    var token:JSONToken;

    /**
     * Constructs a new JSONDecoder to parse a JSON string
     * into a native object.
     *
     * @param s The JSON string to be converted into a native object
     */
    public function new(s:String, strict:Bool) {
        this.strict = strict;
        tokenizer = new JSONTokenizer(s,strict);
        nextToken();
        value = parseValue();
        if (strict && nextToken() != null)
            tokenizer.parseError("Unexpected characters left in input stream!");
    }

    /**
     * Gets the internal object that was created by parsing
     * the JSON string passed to the constructor.
     *
     * @return The internal object representation of the JSON
     *         string that was passed to the constructor
     */
    public function getValue():Dynamic {
        return value;
    }

    /**
     * Returns the next token from the tokenzier reading
     * the JSON string
     */
    function nextToken():JSONToken {
        return token = tokenizer.getNextToken();
    }

    /**
     * Attempt to parse an array
     */
    function parseArray():Array < Dynamic > {
        // create an array internally that we're going to attempt
        // to parse from the tokenizer
        var a:Array<Dynamic> = new Array<Dynamic>();
        // grab the next token from the tokenizer to move
        // past the opening [
        nextToken();
        // check to see if we have an empty array
        if ( token.type == RIGHT_BRACKET ) {
            // we're done reading the array, so return it
            return a;
        }
        else {
            if (!strict && token.type == JSONTokenType.COMMA) {
                nextToken();
                // check to see if we're reached the end of the array
                if ( token.type == JSONTokenType.RIGHT_BRACKET ){
                    return a;
                }
                else {
                    tokenizer.parseError( "Leading commas are not supported.  Expecting ']' but found " + token.value );
                }
            }
        }
        // deal with elements of the array, and use an "infinite"
        // loop because we could have any amount of elements
        while ( true ) {
            // read in the value and add it to the array
            a.push ( parseValue() );
            // after the value there should be a ] or a ,
            nextToken();
            if ( token.type == RIGHT_BRACKET ) {
                // we're done reading the array, so return it
                return a;
            } else if ( token.type == COMMA ) {
                // move past the comma and read another value
                nextToken();
                // Allow arrays to have a comma after the last element
                // if the decoder is not in strict mode
                if ( !strict ){
                    // Reached ",]" as the end of the array, so return it
                    if ( token.type == JSONTokenType.RIGHT_BRACKET ){
                        return a;
                    }
                }
            } else {
                tokenizer.parseError( "Expecting ] or , but found " + token.value );
            }
        }
        return null;
    }

    /**
     * Attempt to parse an object
     */
    function parseObject():Dynamic {
        // create the object internally that we're going to
        // attempt to parse from the tokenizer
        var o:Dynamic = { };
        // store the string part of an object member so
        // that we can assign it a value in the object
        var key:String;
        // grab the next token from the tokenizer
        nextToken();
        // check to see if we have an empty object
        if ( token.type == RIGHT_BRACE ) {
            // we're done reading the object, so return it
            return o;
        }    // in non-strict mode an empty object is also a comma
            // followed by a right bracket
        else {
            if ( !strict && token.type == JSONTokenType.COMMA )    {
                // move past the comma
                nextToken();
                // check to see if we're reached the end of the object
                if ( token.type == JSONTokenType.RIGHT_BRACE )                {
                    return o;
                }
                else {
                    tokenizer.parseError( "Leading commas are not supported.  Expecting '}' but found " + token.value );
                }
            }
        }
        // deal with members of the object, and use an "infinite"
        // loop because we could have any amount of members
        while ( true ) {
            if ( token.type == STRING ) {
                // the string value we read is the key for the object
                key = Std.string(token.value);
                // move past the string to see what's next
                nextToken();
                // after the string there should be a :
                if ( token.type == COLON ) {
                    // move past the : and read/assign a value for the key
                    nextToken();
                    Reflect.setField(o,key,parseValue());
                    // move past the value to see what's next
                    nextToken();
                    // after the value there's either a } or a ,
                    if ( token.type == RIGHT_BRACE ) {
                        // // we're done reading the object, so return it
                        return o;
                    } else if ( token.type == COMMA ) {
                        // skip past the comma and read another member
                        nextToken();

                        // Allow objects to have a comma after the last member
                        // if the decoder is not in strict mode
                        if ( !strict ){
                            // Reached ",}" as the end of the object, so return it
                            if ( token.type == JSONTokenType.RIGHT_BRACE )    {
                                return o;
                            }
                        }
                    } else {
                        tokenizer.parseError( "Expecting } or , but found " + token.value );
                    }
                } else {
                    tokenizer.parseError( "Expecting : but found " + token.value );
                }
            } else {
                tokenizer.parseError( "Expecting string but found " + token.value );
            }
        }
        return null;
    }

    /**
     * Attempt to parse a value
     */
    function parseValue():Dynamic    {
        // Catch errors when the input stream ends abruptly
        if ( token == null )
            tokenizer.parseError( "Unexpected end of input" );
        switch ( token.type ) {
            case LEFT_BRACE:
                return parseObject();
            case LEFT_BRACKET:
                return parseArray();
            case STRING:
                return token.value;
            case NUMBER:
                return token.value;
            case TRUE:
                return true;
            case FALSE:
                return false;
            case NULL:
                return null;
            case NAN:
                if (!strict)
                    return token.value;
                else
                    tokenizer.parseError( "Unexpected " + token.value );
            default:
                tokenizer.parseError( "Unexpected " + token.value );
        }
        return null;
    }
}


private class JSONEncoder {
    /** The string that is going to represent the object we're encoding */
    var jsonString:String;

    /**
     * Creates a new JSONEncoder.
     *
     * @param o The object to encode as a JSON string
     */
    public function new(value:Dynamic) {
        jsonString = convertToString(value);
    }

    /**
     * Gets the JSON string from the encoder.
     *
     * @return The JSON string representation of the object
     *         that was passed to the constructor
     */
    public function getString():String {
        return jsonString;
    }

    /**
     * Converts a value to it's JSON string equivalent.
     *
     * @param value The value to convert.  Could be any
     *        type (object, number, array, etc)
     */
    function convertToString(value:Dynamic):String {
        if (Std.is(value, List) || Std.is(value,IntHash))
            value = Lambda.array(value);
        if (Std.is(value, Hash))
            value = mapHash(value);
        // determine what value is and convert it based on it's type
        if ( Std.is(value,String )) {
            // escape the string so it's formatted correctly
            return escapeString(cast(value, String));
            //return escapeString( value as String );
        } else if ( Std.is(value,Float) ) {
            // only encode numbers that finate
            return Math.isFinite(cast(value,Float)) ? value+"" : "null";
        } else if ( Std.is(value,Bool) ) {
            // convert boolean to string easily
            return value ? "true" : "false";
        } else if ( Std.is(value,Array)) {
            // call the helper method to convert an array
            return arrayToString(cast(value,Array<Dynamic>));
        } else if (Std.is(value,Dynamic) && value != null ) {
            // call the helper method to convert an object
            return objectToString( value );
        }
        return "null";
    }

    function mapHash(value:Hash<Dynamic>):Dynamic{
        var ret:Dynamic = { };
        for (i in value.keys())
            Reflect.setField(ret, i, value.get(i));
        return ret;
    }

    /**
     * Escapes a string accoding to the JSON specification.
     *
     * @param str The string to be escaped
     * @return The string with escaped special characters
     *         according to the JSON specification
     */
    function escapeString( str:String ):String {
        // create a string to store the string's jsonstring value
        var s:String = "";
        // current character in the string we're processing
        var ch:String;
        // store the length in a local variable to reduce lookups
        var len:Int = str.length;
        #if neko
        var utf8mode = (Utf8.length(str)!=str.length);
        if (utf8mode)
            len = Utf8.length(str);
        #elseif php
        var utf8mode = (Utf8.length(str)!=str.length);
        if (utf8mode)
            len = Utf8.length(str);
        #end
        // loop over all of the characters in the string
        for (i in 0...len) {
            // examine the character to determine if we have to escape it
            ch = str.charAt( i );
            #if neko
            if (utf8mode) {
                ch = Utf8.sub(str,i,1);
            }
            #elseif php
            if (utf8mode) {
                ch = Utf8.sub(str,i,1);
            }
            #end
            switch ( ch ) {
                case '"':    // quotation mark
                    s += "\\\"";
                case '\\':    // reverse solidus
                    s += "\\\\";
                case '\n':    // newline
                    s += "\\n";
                case '\r':    // carriage return
                    s += "\\r";
                case '\t':    // horizontal tab
                    s += "\\t";
                default:    // everything else
                    // check for a control character and escape as unicode
                    var code = ch.charCodeAt(0);
                    #if neko
                    if (utf8mode)
                        code = Utf8.charCodeAt(str,i);
                    #elseif php
                    if (utf8mode)
                        code = Utf8.charCodeAt(str,i);
                    #end
                    if ( ch < ' ' || code > 127) {
                        // get the hex digit(s) of the character (either 1 or 2 digits)
                        #if neko
                        var hexCode:String = StringTools.hex(Utf8.charCodeAt(str,i));
                        #elseif php
                        var hexCode:String = StringTools.hex(Utf8.charCodeAt(str,i));
                        #else
                        var hexCode:String = StringTools.hex(ch.charCodeAt( 0 ));
                        #end
                        // ensure that there are 4 digits by adjusting
                        // the # of zeros accordingly.
                        var zeroPad:String = "";
                        for (j in 0...4 - hexCode.length) {
                            zeroPad += "0" ;
                        }
                        //var zeroPad:String = hexCode.length == 2 ? "00" : "000";
                        // create the unicode escape sequence with 4 hex digits
                        s += "\\u" + zeroPad + hexCode;
                    } else {
                        // no need to do any special encoding, just pass-through
                        s += ch;
                    }
            }    // end switch
        }    // end for loop
        return "\"" + s + "\"";
    }

    /**
     * Converts an array to it's JSON string equivalent
     *
     * @param a The array to convert
     * @return The JSON string representation of <code>a</code>
     */
    function arrayToString( a:Array < Dynamic > ):String {
        //trace("arrayToString");
        // create a string to store the array's jsonstring value
        var s:String = "";
        // loop over the elements in the array and add their converted
        // values to the string
        for (i in 0...a.length) {
            // when the length is 0 we're adding the first element so
            // no comma is necessary
            if ( s.length > 0 ) {
                // we've already added an element, so add the comma separator
                s += ",";
            }
            // convert the value to a string
            s += convertToString( a[i] );
        }

        // KNOWN ISSUE:  In ActionScript, Arrays can also be associative
        // objects and you can put anything in them, ie:
        //        myArray["foo"] = "bar";
        //
        // These properties aren't picked up in the for loop above because
        // the properties don't correspond to indexes.  However, we're
        // sort of out luck because the JSON specification doesn't allow
        // these types of array properties.
        //
        // So, if the array was also used as an associative object, there
        // may be some values in the array that don't get properly encoded.
        //
        // A possible solution is to instead encode the Array as an Object
        // but then it won't get decoded correctly (and won't be an
        // Array instance)

        // close the array and return it's string value
        return "[" + s + "]";
    }

    /**
     * Converts an object to it's JSON string equivalent
     *
     * @param o The object to convert
     * @return The JSON string representation of <code>o</code>
     */
    function objectToString(o:Dynamic):String {
        //trace("objectToString");
        //trace(o);
        // create a string to store the object's jsonstring value
        var s:String = "";
        var value:Dynamic;
        // loop over the keys in the object and add their converted
        // values to the string
        for ( key in Reflect.fields(o) ) {
            // assign value to a variable for quick lookup
            value = Reflect.field(o,key);
            // don't add function's to the JSON string
            if (!Reflect.isFunction(value))    {
                // when the length is 0 we're adding the first item so
                // no comma is necessary
                if ( s.length > 0 ) {
                    // we've already added an item, so add the comma separator
                    s += ",";
                }
                s += escapeString( key ) + ":" + convertToString( value );
            }
        }
        return "{" + s + "}";
    }
}

private class JSONTokenizer {
    /** The object that will get parsed from the JSON string */
    var obj:Dynamic;
    /** The JSON string to be parsed */
    var jsonString:String;
    /** The current parsing location in the JSON string */
    var loc:Int;
    /** The current character in the JSON string during parsing */
    var ch:String;
    var strict:Bool;

    /**
     * Constructs a new JSONDecoder to parse a JSON string
     * into a native object.
     *
     * @param s The JSON string to be converted
     *        into a native object
     */
    public function new(s:String,strict:Bool) {
        jsonString = s;
        this.strict = strict;
        loc = 0;
        // prime the pump by getting the first character
        nextChar();
    }

    /**
     * Gets the next token in the input sting and advances
    * the character to the next character after the token
     */
    public function getNextToken():JSONToken {
        var token:JSONToken = new JSONToken();
        // skip any whitespace / comments since the last
        // token was read
        skipIgnored();
        // examine the new character and see what we have...
        switch ( ch ) {
            case '{':
                token.type = LEFT_BRACE;
                token.value = '{';
                nextChar();
            case '}':
                token.type = RIGHT_BRACE;
                token.value = '}';
                nextChar();
            case '[':
                token.type = LEFT_BRACKET;
                token.value = '[';
                nextChar();
            case ']':
                token.type = RIGHT_BRACKET;
                token.value = ']';
                nextChar();
            case ',':
                token.type = COMMA;
                token.value = ',';
                nextChar();
            case ':':
                token.type = COLON;
                token.value = ':';
                nextChar();
            case 't': // attempt to read true
                var possibleTrue:String = "t" + nextChar() + nextChar() + nextChar();
                if ( possibleTrue == "true" ) {
                    token.type = TRUE;
                    token.value = true;
                    nextChar();
                } else {
                    parseError( "Expecting 'true' but found " + possibleTrue );
                }
            case 'f': // attempt to read false
                var possibleFalse:String = "f" + nextChar() + nextChar() + nextChar() + nextChar();
                if ( possibleFalse == "false" ) {
                    token.type = FALSE;
                    token.value = false;
                    nextChar();
                } else {
                    parseError( "Expecting 'false' but found " + possibleFalse );
                }
            case 'n': // attempt to read null
                var possibleNull:String = "n" + nextChar() + nextChar() + nextChar();
                if ( possibleNull == "null" ) {
                    token.type = NULL;
                    token.value = null;
                    nextChar();
                } else {
                    parseError( "Expecting 'null' but found " + possibleNull );
                }
            case 'N': //attempt to read NAN
                var possibleNAN:String = 'N' + nextChar() + nextChar();
                if (possibleNAN == "NAN" || possibleNAN == "NaN") {
                    token.type = NAN;
                    token.value = Math.NaN;
                    nextChar();
                }
                else {
                    parseError("Expecting 'nan' but found " + possibleNAN);
                }
            case '"': // the start of a string
                token = readString();
            default:
                // see if we can read a number
                if ( isDigit( ch ) || ch == '-' ) {
                    token = readNumber();
                } else if ( ch == '' ) {
                    // check for reading past the end of the string
                    return null;
                } else {
                    // not sure what was in the input string - it's not
                    // anything we expected
                    parseError( "Unexpected " + ch + " encountered" );
                }
        }
        return token;
    }

    /**
     * Attempts to read a string from the input string.  Places
     * the character location at the first character after the
     * string.  It is assumed that ch is " before this method is called.
     *
     * @return the JSONToken with the string value if a string could
     *        be read.  Throws an error otherwise.
     */
    function readString():JSONToken {
        // the string to store the string we'll try to read
        var string:String = "";
        // advance past the first "
        nextChar();
        while ( ch != '"' && ch != '' ) {
            //trace(ch);
            // unescape the escape sequences in the string
            if ( ch == '\\' ) {
                // get the next character so we know what
                // to unescape
                nextChar();
                switch ( ch ) {
                    case '"': // quotation mark
                        string += '"';
                    case '/':    // solidus
                        string += "/";
                    case '\\':    // reverse solidus
                        string += '\\';
                    case 'n':    // newline
                        string += '\n';
                    case 'r':    // carriage return
                        string += '\r';
                    case 't':    // horizontal tab
                        string += '\t';
                    case 'u':
                        // convert a unicode escape sequence
                        // to it's character value - expecting
                        // 4 hex digits
                        // save the characters as a string we'll convert to an int
                        var hexValue:String = "";
                        // try to find 4 hex characters
                        for (i in 0...4) {
                            // get the next character and determine
                            // if it's a valid hex digit or not
                            if ( !isHexDigit( nextChar() ) ) {
                                parseError( " Excepted a hex digit, but found: " + ch );
                            }
                            // valid, add it to the value
                            hexValue += ch;
                        }
                        // convert hexValue to an integer, and use that
                        // integrer value to create a character to add
                        // to our string.
                        //string += String.fromCharCode( Std.parseInt( hexValue)); //, 16 ) );
                        #if neko
                        var utf = new Utf8(1);
                        utf.addChar(hexValToInt(hexValue));
                        string += utf.toString();
                        #elseif php
                        var utf = new Utf8();
                        utf.addChar(hexValToInt(hexValue));
                        string += utf.toString();
                        //trace(string);
                        #else
                        string += String.fromCharCode(hexValToInt(hexValue));
                        #end
                    default:
                        // couldn't unescape the sequence, so just
                        // pass it through
                        string += '\\' + ch;
                }
            } else {
                // didn't have to unescape, so add the character to the string
                string += ch;
            }
            // move to the next character
            nextChar();
        }

        // we read past the end of the string without closing it, which
        // is a parse error
        if ( ch == '' ) {
            parseError( "Unterminated string literal" );
        }
        // move past the closing " in the input string
        nextChar();
        // the token for the string we'll try to read
        var token:JSONToken = new JSONToken();
        token.type = STRING;
        // attach to the string to the token so we can return it
        token.value = string;
        return token;
    }

    inline function hexValToInt(hexVal:String):Int {
        var ret:Int = 0;
        for (i in 0...hexVal.length) {
            ret = ret << 4;
            switch (hexVal.charAt(i).toUpperCase()) {
                case "1":ret += 1;
                case "2":ret += 2;
                case "3":ret += 3;
                case "4":ret += 4;
                case "5":ret += 5;
                case "6":ret += 6;
                case "7":ret += 7;
                case "8":ret += 8;
                case "9":ret += 9;
                case "A":ret += 10;
                case "B":ret += 11;
                case "C":ret += 12;
                case "D":ret += 13;
                case "E":ret += 14;
                case "F":ret += 15;
            }
        }
        return ret;
    }

    /**
     * Attempts to read a number from the input string.  Places
     * the character location at the first character after the
     * number.
     *
     * @return The JSONToken with the number value if a number could
     *         be read.  Throws an error otherwise.
     */
    function readNumber():JSONToken {
        // the string to accumulate the number characters
        // into that we'll convert to a number at the end
        var input:String = "";
        // check for a negative number
        if ( ch == '-' ) {
            input += '-';
            nextChar();
        }
        // the number must start with a digit
        if ( !isDigit( ch ) ) {
            parseError( "Expecting a digit" );
        }
        // 0 can only be the first digit if it
        // is followed by a decimal point
        if ( ch == '0' ){
            input += ch;
            nextChar();
            // make sure no other digits come after 0
            if ( isDigit( ch ) ) {
                parseError( "A digit cannot immediately follow 0" );
            }
            // unless we have 0x which starts a hex number, but this
            // doesn't match JSON spec so check for not strict mode.
            else {
                if (!strict && ch == 'x') {
                    // include the x in the input
                    input += ch;
                    nextChar();
                    // need at least one hex digit after 0x to
                    // be valid
                    if (isHexDigit(ch)) {
                        input += ch;
                        nextChar();
                    }
                    else {
                        parseError( "Number in hex format require at least one hex digit after \"0x\"" );
                    }
                    // consume all of the hex values
                    while (isHexDigit(ch)) {
                        input += ch;
                        nextChar();
                    }
                    input = Std.string(hexValToInt(input));
                }
            }
        }
        else {
            // read numbers while we can
            while ( isDigit( ch ) ) {
                input += ch;
                nextChar();
            }
        }
        // check for a decimal value
        if ( ch == '.' ) {
            input += '.';
            nextChar();
            // after the decimal there has to be a digit
            if ( !isDigit( ch ) ){
                parseError( "Expecting a digit" );
            }
            // read more numbers to get the decimal value
            while ( isDigit( ch ) ) {
                input += ch;
                nextChar();
            }
        }
        // check for scientific notation
        if ( ch == 'e' || ch == 'E' )    {
            input += "e";
            nextChar();
            // check for sign
            if ( ch == '+' || ch == '-' ){
                input += ch;
                nextChar();
            }
            // require at least one number for the exponent
            // in this case
            if ( !isDigit( ch ) ){
                parseError( "Scientific notation number needs exponent value" );
            }
            // read in the exponent
            while ( isDigit( ch ) )    {
                input += ch;
                nextChar();
            }
        }
        // convert the string to a number value
        var num:Float = Std.parseFloat(input);
        if ( Math.isFinite( num ) && !Math.isNaN( num ) ) {
            // the token for the number we'll try to read
            var token:JSONToken = new JSONToken();
            token.type = NUMBER;
            token.value = num;
            return token;
        } else {
            parseError( "Number " + num + " is not valid!" );
        }
        return null;
    }

    /**
     * Reads the next character in the input
     * string and advances the character location.
     *
     * @return The next character in the input string, or
     *        null if we've read past the end.
     */
    function nextChar():String {
        return ch = jsonString.charAt( loc++ );
    }

    /**
     * Advances the character location past any
     * sort of white space and comments
     */
    function skipIgnored():Void    {
        var originalLoc:Int;
        // keep trying to skip whitespace and comments as long
        // as we keep advancing past the original location
        do {
            originalLoc = loc;
            skipWhite();
            skipComments();
        }while ( originalLoc != loc );
    }

    /**
     * Skips comments in the input string, either
     * single-line or multi-line.  Advances the character
     * to the first position after the end of the comment.
     */
    function skipComments():Void {
        if ( ch == '/' ) {
            // Advance past the first / to find out what type of comment
            nextChar();
            switch ( ch ) {
                case '/': // single-line comment, read through end of line
                    // Loop over the characters until we find
                    // a newline or until there's no more characters left
                    do {
                        nextChar();
                    } while ( ch != '\n' && ch != '' );
                    // move past the \n
                    nextChar();
                case '*': // multi-line comment, read until closing */
                    // move past the opening *
                    nextChar();
                    // try to find a trailing */
                    while ( true ) {
                        if ( ch == '*' ) {
                            // check to see if we have a closing /
                            nextChar();
                            if ( ch == '/') {
                                // move past the end of the closing */
                                nextChar();
                                break;
                            }
                        } else {
                            // move along, looking if the next character is a *
                            nextChar();
                        }
                        // when we're here we've read past the end of
                        // the string without finding a closing */, so error
                        if ( ch == '' ) {
                            parseError( "Multi-line comment not closed" );
                        }
                    }
                // Can't match a comment after a /, so it's a parsing error
                default:
                    parseError( "Unexpected " + ch + " encountered (expecting '/' or '*' )" );
            }
        }
    }


    /**
     * Skip any whitespace in the input string and advances
     * the character to the first character after any possible
     * whitespace.
     */
    function skipWhite():Void {
        // As long as there are spaces in the input
        // stream, advance the current location pointer
        // past them
        while ( isWhiteSpace( ch ) ) {
            nextChar();
        }
    }

    /**
     * Determines if a character is whitespace or not.
     *
     * @return True if the character passed in is a whitespace
     *    character
     */
    function isWhiteSpace( ch:String ):Bool {
        return ( ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' );
    }

    /**
     * Determines if a character is a digit [0-9].
     *
     * @return True if the character passed in is a digit
     */
    function isDigit( ch:String ):Bool {
        #if php
        return (ch >= '0' && ch <= '9' && ch!='');
        #else
        return ( ch >= '0' && ch <= '9' );
        #end
    }

    /**
     * Determines if a character is a digit [0-9].
     *
     * @return True if the character passed in is a digit
     */
    function isHexDigit( ch:String ):Bool {
        // get the uppercase value of ch so we only have
        // to compare the value between 'A' and 'F'
        var uc:String = ch.toUpperCase();
        // a hex digit is a digit of A-F, inclusive ( using
        // our uppercase constraint )
        return ( isDigit( ch ) || ( uc >= 'A' && uc <= 'F' ) );
    }

    /**
     * Raises a parsing error with a specified message, tacking
     * on the error location and the original string.
     *
     * @param message The message indicating why the error occurred
     */
    public function parseError( message:String ):Void {
        throw new JSONParseError( message, loc, jsonString );
    }
}

private class JSONToken {
    /** type of the token */
    public var type:JSONTokenType;
    /** value of the token */
    public var value:Dynamic;

    /**
     * Creates a new JSONToken with a specific token type and value.
     *
     * @param type The JSONTokenType of the token
     * @param value The value of the token
     */
    public function new(?type:JSONTokenType,?value:Dynamic = null) {
        this.type = type==null?UNKNOWN:type;
        this.value = value;
    }
}

private class JSONParseError {
    /** The location in the string where the error occurred */
    var _location:Int;
    /** The string in which the parse error occurred */
    var _text:String;
    var name:String;
    var message:String;

    public var text(gettext, null):String;
    public var location(getlocation, null):Int;

    /**
     * Constructs a new JSONParseError.
     *
     * @param message The error message that occured during parsing
     */
    public function new(message:String = "",location:Int = 0,text:String = "") {
        //super( message );
        name = "JSONParseError";
        _location = location;
        _text = text;
        this.message = message;
    }

    /**
     * Provides read-only access to the location variable.
     *
     * @return The location in the string where the error occurred
     */
    public function getlocation():Int {return _location;}

    /**
     * Provides read-only access to the text variable.
     *
     * @return The string in which the error occurred
     */
    public function gettext():String {return _text;}

    public function toString():String {
        return name + ": " + message + " at position: " + _location + " near \"" + _text+"\"";
    }
}

