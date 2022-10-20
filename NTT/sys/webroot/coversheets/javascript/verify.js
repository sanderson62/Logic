// A utility that clears a form field on focus.
function clearBox(field)
{
	field.value = "";
}


// A utility that sets field form_nav to "FWD"
function navfwd()
{
	UpdateForm.form_nav.value = "FWD";
}
// A utility that sets field form_nav to "BWD"
function navbwd()
{
	UpdateForm.form_nav.value = "BWD";
}

// A utility that sets field form_nav to "DEL"
function delform()
{
	UpdateForm.form_nav.value = "DEL";
}

// A utility function that returns true if a string contains only 
// whitespace characters.
function isblank(s)
{
    for(var i = 0; i < s.length; i++) {
        var c = s.charAt(i);
        if ((c != ' ') && (c != '\n') && (c != '\t')) return false;
    }
    return true;
}

// This function verifies that the input value is a number by
// checking each character.
function isANumber(field)
{
    for (var i = 0; i < field.length; i++) {
       if ((field.charAt(i) != "0") && (!parseFloat(field.charAt(i)))) 
          return false;
    }
    return true;
}

// This is the function that performs form verification. It will be invoked
// from the onSubmit() event handler. The handler should return whatever
// value this function returns.
function verify(f)
{
    var msg;
    var empty_fields = "";
    var errors = "";


    // Loop through the elements of the form, looking for all 
    // text and textarea elements that don't have an "optional" property
    // defined. Then, check for fields that are empty and make a list of them.
    // Also, if any of these elements have a "min" or a "max" property defined,
    // then verify that they are numbers and that they are in the right range.
    // Put together error messages for fields that are wrong.
    for(var i = 0; i < f.length; i++) {
        var e = f.elements[i];
        if (((e.type == "text") || (e.type == "textarea")) && !e.optional) {
            // first check if the field is empty
            if ((e.value == null) || (e.value == "") || isblank(e.value)) {
                empty_fields += "\n          " + e.name;
                continue;
            }
            // Now check for fields that are supposed to be numeric.
      //    if (e.numeric || (e.min != null) || (e.max != null)) { 
      //        if (!isANumber(e.value) || 
      //            ((e.min != null) && (e.value < e.min)) || 
      //            ((e.max != null) && (e.value > e.max))) {
      //            errors += "* The field " + e.name + " must be a number";
      //            if (e.min != null) 
      //                errors += " that is greater than " + e.min;
      //            if (e.max != null && e.min != null) 
      //                errors += " and less than " + e.max;
      //            else if (e.max != null)
      //                errors += " that is less than " + e.max;
      //            errors += ".\n";
      //        }
      //    }
        }
    }
    // Now, if there were any errors, then display the messages, and
    // return false to prevent the form from being submitted. Otherwise
    // return true.
    if (!empty_fields && !errors) return true;
    msg  = "______________________________________________________\n\n"
    msg += "The form was not submitted because of the following error(s).\n";
    msg += "Please correct these error(s) and re-submit.\n";
    msg += "______________________________________________________\n\n"
    if (empty_fields) {
        msg += "* The following required field(s) are empty:" 
                + empty_fields + "\n";
        if (errors) msg += "\n";
    }
    msg += errors;
    alert(msg);
    return false;
}
