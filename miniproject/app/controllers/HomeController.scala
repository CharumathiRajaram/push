package controllers
import newsample.demo.com.{CsvEmptyFileException, CsvFile, CsvFileNotFoundException, InvalidFileException, JsonEmptyFileException, JsonFile, JsonFileNotFoundException, Static}
import javax.inject._
import play.api.libs.json.{JsResultException, Json}
import play.api.mvc._
import java.io.File
import scala.collection.mutable.LinkedHashMap
@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {
  var csvObj:CsvFile=_ //global object declaration for  accessing csv file
  var jsonObj:JsonFile=_//global object declaration for  accessing json file
  val PASS_INPUT_JSON="Pass values in body in json format"
  var flag=false//for compare function,it will be updated as true when compare function get success
  def menu()=Action{ implicit request =>
    val menu=Json.obj("/uploadingFile"->"To upload Files ",
      "/compare"->"To compare CSV and JSON files",
      "/viewCsv"->"To view CSV file",
      "/viewJson"->"To view a JSON file",
      "/rowCol"->"To display number of rows and columns",
      "/columnHeadings"->"To display the column headers in the dataset",
      "/topNRows"->"To display top n rows in the dataset",
      "/filterFunc"->"To do a column wise filtering",
      "/maximum"->"To find a maximum value",
      "/minimum"->"To find a minimum value",
      "/average"->"To find a average value",
      "/isContains"->"To check whether a column contains a entered string")
    Ok(Json.obj("Menu"->menu))
  }
  def keyValue(headers:Vector[String], values:Vector[Vector[String]]) = {
    var valuesMap: Vector[LinkedHashMap[String, String]] = Vector()
    values.foreach {
      case row => valuesMap = valuesMap :+ LinkedHashMap(headers zip row: _*)
    }
    valuesMap
  }

  //checking file is exist
  def checkFile(file:File): Boolean = {
    if (file.exists())  true
    else false
  }
  //checking file is exist
  def isEmpty(file: File):Boolean ={
    if (file.length()>0) true
    else false
  }
  //file upload function
  def fileUpload()=Action{ implicit request =>
      if (request.hasBody) {                                          //checks request has body
        val fileBody = request.body.asJson
        try {
          val csvPath = fileBody.get("csvPath").as[String]            //takes values from the body  having key value as csvPath
          val jsonPath = fileBody.get("jsonPath").as[String]          //takes values from the body  having key value as csvPath
          val file = new File(csvPath)                                //passing csv path to file
          val file1 = new File(jsonPath)                              //passing json path to file
          if(csvPath.contains(".csv")&& jsonPath.contains(".json")){  //checks the file has proper extension
            if (!checkFile(file)) throw new CsvFileNotFoundException
            if (!checkFile(file1)) throw new JsonFileNotFoundException
            if (!isEmpty(file)) throw new CsvEmptyFileException
            if (!isEmpty(file1)) throw new JsonEmptyFileException
            csvObj = new CsvFile(file)                                      //updating object with passing file b
            jsonObj = new JsonFile(file1)                                   //updating object with passing file
            if(csvObj.header.head.trim.isEmpty)  BadRequest(Json.obj(Static.ERROR->Static.EMPTY_VALUES,Static.STATUS ->Static.UNABLE_PROCESS))      //checks csv file contains space as values
            if (jsonObj.valid != Static.SUCCESS)    BadRequest(Json.obj(Static.EXCEPTION->jsonObj.valid,Static.STATUS ->Static.WARNING))            //checks it is valid json or not
            else if(csvObj.rowValues.isEmpty)   BadRequest(Json.obj(Static.ERROR->"only header found,no values found in dataset",Static.STATUS ->Static.UNABLE_PROCESS)) //checks values aval
            else  Ok(Json.obj(Static.SUCCESS -> " Both File uploaded successfully",Static.STATUS->Static.UPLOAD_SUCCESS))
          }else throw new InvalidFileException
        } catch {
          case _   :JsResultException=>BadRequest(Json.obj(Static.INVALID_INPUT->"Check the input value  in string format"))
          case ife :InvalidFileException=>UnsupportedMediaType(Json.obj(Static.EXCEPTION->ife.exception,Static.STATUS->Static.UPLOAD_FAILED))
          case jnfe: JsonFileNotFoundException => NotFound(Json.obj(Static.EXCEPTION->jnfe.exception,Static.STATUS->Static.UPLOAD_FAILED))
          case cnfe: CsvFileNotFoundException => NotFound(Json.obj(Static.EXCEPTION->cnfe.exception,Static.STATUS->Static.UPLOAD_FAILED))
          case cefe: CsvEmptyFileException => BadRequest(Json.obj(Static.EXCEPTION->cefe.exception,Static.STATUS->Static.UPLOAD_FAILED))
          case jefe: JsonEmptyFileException => BadRequest(Json.obj(Static.EXCEPTION->jefe.exception,Static.STATUS->Static.UPLOAD_FAILED))
         case _: NoSuchElementException => BadRequest(Json.obj(Static.EXCEPTION->Static.INVAID_INPUT_KEY))
        }
      }
      else  BadRequest(Json.obj(Static.ERROR->PASS_INPUT_JSON))
  }

  //compare function
  def compare()=Action{
    try {
      val result = csvObj.compareFile(jsonObj.jsonFieldName)
      if (result._1 == true) {
        flag = true
        Ok(Json.obj(Static.STATUS -> result._2))
      } else BadRequest(Json.obj(Static.ERROR -> result._2, "Not match fields" -> result._3))
    }catch {
      case _: NullPointerException => BadRequest(Json.obj(Static.ERROR -> Static.RE_UPLOAD))
    }
  }

  //viewing csv file
  def viewCsv()=Action { implicit request =>
    try {
      Ok(Json.obj("Column Name" -> csvObj.header, "Values" -> keyValue(csvObj.header,csvObj.rowValues)))
    } catch {
      case _: NullPointerException => BadRequest(Json.obj(Static.ERROR -> Static.RE_UPLOAD))
    }
  }

  //viewing json file
  def viewJson()=Action { implicit request =>
    try {
      Ok(jsonObj.jsonRead)
    }catch {
      case _: NullPointerException => BadRequest(Json.obj(Static.ERROR-> Static.RE_UPLOAD))
    }
  }

  //getting headers
  def colHeader()=Action{ implicit request=>
   // if(flag==true)
    Ok(Json.obj("column name"->csvObj.header))
    //else BadRequest("json not match with csv so you cannot access this method")
  }

  //getting total number of rows and columns
  def rowCol()=Action{implicit request=>
    val result =csvObj.rowColumn
    Ok(Json.obj("Total number of Rows"->result._2,"Total number of Column"->result._1))
  }

  //getting top n rows
  def topRows()=Action{implicit request=>
    if(request.hasBody){
      val fileBody = request.body.asJson
      try {
        val num = fileBody.get("num_of_rows").as[Int]
          val result = csvObj.topNRows(num)
          if (num > csvObj.rowValues.length) BadRequest(Json.obj(Static.WARNING-> result._2))
          else BadRequest(Json.obj(s"Top $num Rows " ->keyValue(csvObj.header,result._1)))
      }catch {
        case _:JsResultException=>BadRequest(Json.obj(Static.INVALID_INPUT->"Check the input value ,give as number"))
        case _: NoSuchElementException => BadRequest(Json.obj(Static.EXCEPTION->Static.INVAID_INPUT_KEY))
      }
    }
    else BadRequest(Json.obj(Static.ERROR->PASS_INPUT_JSON))
  }
  //minimum function
  def minFun()=Action { implicit request =>
    if(flag==true) {
      if (request.hasBody) {
        val fileBody = request.body.asJson
        try {
          val colName = fileBody.get("colName").as[String]
          val result = csvObj.minFunction(jsonObj.numericColumn, colName)
          if (result._1 == Static.SUCCESS) Ok(Json.obj("column Name" -> csvObj.header, "Rows Found"->result._2.length,"Minimum value array " ->keyValue(csvObj.header,result._2)))
          else BadRequest(Json.obj(Static.ERROR -> result._1))
        }
        catch {
          case jre: JsResultException => BadRequest(Json.obj(Static.INVALID_INPUT -> "Check the input value ,give string as input"))
          case _: NoSuchElementException => BadRequest(Json.obj(Static.EXCEPTION -> Static.INVAID_INPUT_KEY))
        }
      }
      else BadRequest(Json.obj(Static.ERROR -> PASS_INPUT_JSON))
    }else BadRequest(Json.obj(Static.ERROR->Static.FAIL_COMPARISION))
  }
//maximum function
  def maxFun()=Action { implicit request =>
    if(flag==true) {
    if (request.hasBody) {
      val fileBody = request.body.asJson
      try {
        val colName = fileBody.get("colName").as[String]
        val result = csvObj.maxFunction(jsonObj.numericColumn, colName)
        if (result._1 == Static.SUCCESS) Ok(Json.obj("column name" -> csvObj.header,"Rows Found"->result._2.length, "Maximum value array " ->  keyValue(csvObj.header,result._2)))
        else BadRequest(Json.obj(Static.ERROR -> result._1))
      }
      catch {
        case jre: JsResultException => BadRequest(Json.obj(Static.INVALID_INPUT -> "Check the input value,pass as string"))
        case _: NoSuchElementException => BadRequest(Json.obj(Static.EXCEPTION -> Static.INVAID_INPUT_KEY))
      }
    }
    else BadRequest(Json.obj(Static.ERROR->PASS_INPUT_JSON))
     }else BadRequest(Json.obj(Static.ERROR->Static.FAIL_COMPARISION))

  }
  //average function
  def average()=Action { implicit request =>
    if (flag == true) {
      if (request.hasBody) {
        val fileBody = request.body.asJson
        try {
          val colName = fileBody.get("colName").as[String]
          val result = csvObj.averageFunction(jsonObj.numericColumn, colName)
          if (result._1 == Static.SUCCESS) Ok(Json.obj("Average for " -> colName, "Average value is" -> result._2))
          else BadRequest(Json.obj(Static.ERROR -> result._1))
        }
        catch {
          case jre: JsResultException => BadRequest(Json.obj(Static.INVALID_INPUT -> "Check the input value,pass as string"))
          case _: NoSuchElementException => BadRequest(Json.obj(Static.EXCEPTION -> Static.INVAID_INPUT_KEY))
        }
      }
      else BadRequest(Json.obj(Static.ERROR -> PASS_INPUT_JSON))
    } else BadRequest(Json.obj(Static.ERROR -> Static.FAIL_COMPARISION))

  }

  //string contains function
  def stringOperation()=Action{implicit request=>
      if (request.hasBody) {
        val fileBody = request.body.asJson
        try {
          val colName = fileBody.get("colName").as[String]
         val start=fileBody.get("startsWith").as[String]
          val end=fileBody.get("endsWith").as[String]
          val contain=fileBody.get("contain").as[String]
          val result = csvObj.stringFunction(colName,start,end,contain,jsonObj.stringColumn)
          if (result._1 == Static.SUCCESS) Ok(Json.obj("Rowsfound"->result._2,"column name" -> result._3, "filtered value is" -> keyValue(csvObj.header,result._4)))
          else BadRequest(Json.obj(Static.ERROR -> result._1))
        } catch {
          case jre: JsResultException => BadRequest(Json.obj(Static.INVALID_INPUT -> "Check the input value ,give proper type"))
          case _: NoSuchElementException => BadRequest(Json.obj(Static.EXCEPTION-> Static.INVAID_INPUT_KEY))
        }
      }
      else BadRequest(Json.obj(Static.ERROR -> PASS_INPUT_JSON))
  }

  //filter Function
  def filterFunc()=Action { implicit request =>
    if (flag == true) {
    if (request.hasBody) {
      val fileBody = request.body.asJson
      try {
        val colName = fileBody.get("colName").as[String]
        val filter = fileBody.get("filterValue").as[String]
        val result = csvObj.filterFunction(colName, filter)
        if (result._1 == Static.SUCCESS) Ok(Json.obj("column name" -> result._3, s"Rows Found for the filter value is: ${result._2} from ${csvObj.rowValues.length}" ->  keyValue(result._3,result._4)))
        else BadRequest(Json.obj(Static.ERROR -> result._1))
      } catch {
        case jre: JsResultException => BadRequest(Json.obj(Static.INVALID_INPUT -> "Check the input value type"))
        case _: NoSuchElementException => BadRequest(Json.obj(Static.EXCEPTION -> Static.INVAID_INPUT_KEY))
      }
    }
    else BadRequest(Json.obj(Static.ERROR -> PASS_INPUT_JSON))
  }else BadRequest(Json.obj(Static.ERROR->Static.FAIL_COMPARISION))
  }
}


