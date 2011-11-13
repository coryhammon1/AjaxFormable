/*
	AjaxFormable.scala
	
	@author: Cory Hammon
	9/2/2011
*/

package code.model

import _root_.scala.xml.{NodeSeq, Text, Node}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.record._
import _root_.net.liftweb.record.field._
import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.http.{S, SHtml, js}
import js._
import JsCmds._
import JE._

/*
	A trait that allows a Record to be edited using an automatically generated ajax form.
*/
trait AjaxFormable[T <: Record[T] with AjaxFormable[T]]
  extends Record[T] {
    self: T =>

	def formFields: List[String]				//the fields of the model that can be edited
	def hiddenFields: List[String] = Nil		//any hidden fields that should be included in the form
	def extraFormFields: List[String] = Nil		//any extra form fields that should be included
	def extraForm: NodeSeq = NodeSeq.Empty		//the NodeSeq that displays the extra form fields
	
	/*
		Returns a NodeSeq with each field in formFields displayed as html
		
		@formFields: the fields that should be displayed as html
	*/
	def ajaxFormTemplate = (formFields).map({ field: String =>
		<div>
			<label for={field + "_id"}>{field.capitalize}</label>
			<lift:field name={field}></lift:field>
			<span id={field + "_id_error"}></span>
		</div>
	})

	/*
		Processes the ajax form request.  Returns a JsCmd (javascript command) with either the
		errors that should be displayed, or the results given by success and postSuccess
		
		@success: a function that returns what the browser should do, when the form is processed successfully
		@postSuccess: a function that returns what the browser should do, after the success function is called
		@formId: the html id of the form being processed
	*/
	def ajaxFormProcess(success: T => JsCmd, postSuccess: T => JsCmd, formId: Box[String]): Any = {
		val errors = this.validate
		(formFields ++ extraFormFields).foreach({ field: String =>
			//get the text of any errors that occurred when the form was processed
			val errorText = errors.find { fe: FieldError =>
				//if the error's field id is equal to the current field's
				//id, then this error belongs to that field, so return true
				(fe.field.uniqueFieldId openOr "") equals field + "_id"
			} map { _.msg.toString } getOrElse "" //return the error's text or return "", if there is no error

			//convert the error (if there is one) to a JsCmd
			val error: JsCmd = formId match {
				case Full(id) => { //if form has an id
					//return a js command that finds the form, finds the field's error display,
					//and sets that display to the error's text
					JsRaw(
						String.format("$('#%s').find('#%s_id_error').html('%s');", id, field, errorText)
					)
				}
				//otherwise, if there is no form id
				case _ => SetHtml(field + "_id_error", Text(errorText)) //set field's error display to errorText
			}

			//add the js command to the list of commands the browser should perform
			S.appendJs(error)
		})

		errors match {
			//if there is no error
			case Nil => success(this) & postSuccess(this) //return the results of success and postSuccess
			case _ => Noop	//do nothing
		}
	}

	//Function proxy for ajaxFormProcess, with a default form id value of Empty
	def ajaxFormProcess(success: T => JsCmd, postSuccess: T => JsCmd): Any =
		ajaxFormProcess(success, postSuccess, Empty)

	//Function proxy for ajaxFormProcess, with a default postSuccess value, which does nothing
	def ajaxFormProcess(success: T => JsCmd, formId: Box[String]): Any =
		ajaxFormProcess(success, { obj: T => Noop }, formId)

	//Function proxy for ajaxFormProcess, with a default postSuccess value, which does nothing
	def ajaxFormProcess(success: T => JsCmd): Any = ajaxFormProcess(success, { obj: T => Noop })

	/*
		Returns the model's ajax form as html.
		
		@button: the string value that should be displayed in the submit button
		@success: a function that returns what the browser should do on successful processing 
	*/
	def toAjaxForm(button: Box[String])(success: T => JsCmd): NodeSeq = {
		val formId = Helpers.nextFuncName		//randomly generated formId

		<div id={formId}>{
			extraForm ++ SHtml.ajaxForm(
				meta.toForm(this, ajaxFormTemplate) ++							//display the form template
				SHtml.hidden(() => ajaxFormProcess(success, Full(formId))) ++	//process the ajax form using ajaxFormProcess	
				<input type="submit" value={button openOr "Submit"} />			//the submit button
			)
		}</div>
	}
}

/*
	Trait that provides an ajaxCreationForm function to the Model's MetaRecord object.
*/
trait AjaxFormableMeta[T <: Record[T]
  with AjaxFormable[T]] extends MetaRecord[T] {
	this: MetaRecord[T] with T =>

	def extraClearCreationForm: JsCmd = Noop	//extra command(s) for clearning the ajax form

	/*
		Returns the html for creating an object using an ajax form.
		
		@button: value the submit button should display
		@success: function that should return what the browser should do on success
	*/
    def ajaxCreationForm(button: Box[String])(success: T => JsCmd): NodeSeq = {
		val obj = createRecord
		val formId = Helpers.nextFuncName		//randomly generated

		/*
			Js command that clears the ajax form's html fields.
		*/
        def clearForm(obj: T): JsCmd = {
            (obj.extraFormFields ++ obj.formFields) match {
                case Nil => Noop		//if there are no fields
                case first :: rest => extraClearCreationForm & JsRaw(String.format("""
                    $('#%s').find(':input').each(function() {
                        switch(this.type) {
                            case 'password':
                            case 'select-multiple':
                            case 'select-one':
                            case 'text':
                            case 'textarea':
                                $(this).val("");
                                break;
                            case 'checkbox':
                            case 'radio':
                                this.checked = false;
                                break;
                            case 'file':
                            	$(this).show();
                        }
                    })""", formId)
                )
            }
        }
		
		/* Ajax Form Html */
		<div id={formId}>{
			extraForm ++ SHtml.ajaxForm(
				toForm(obj, ajaxFormTemplate) ++		//the ajax fields html
				SHtml.hidden(() => {
					//happens when ajax request is submitted
					copyObject(obj).ajaxFormProcess(success, clearForm, Full(formId))
				}) ++
				<input type="submit" value={button openOr "Submit"} />
			)
		}</div>
	}
    
	/*
		Return a new model with only the model's form fields copied
	*/
    private def copyObject(obj: T): T = {
    	val newRecord = createRecord
    	
    	val objJValue: JObject = obj.asJValue	//convert the object to JSON format
    	
		//get only the fields in formFields and hiddenFields
    	val fields = objJValue.obj.filter(jField => (formFields ++ hiddenFields) contains jField.name)
    	
		//set the newRecord's fields to the formFields and hiddenFields of the old object
    	newRecord.setFieldsFromJValue(JObject(fields)) openOr {
			//or if an error occurred, copy the whole object
			newRecord.setFieldsFromJValue(obj.asJValue)
    	}
    	
    	newRecord
    }
}