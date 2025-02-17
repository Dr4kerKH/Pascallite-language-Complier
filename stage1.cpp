// Name: Sonpaorong Muchhim
// CS 4301
// Complier Project: Stage1

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <ctime>
#include <cctype>
#include <stage1.h>
#include <algorithm>
#include <cstdio>
#include <iomanip>
#include <sstream>
using namespace std;

// ********************** Helper functions for the Pascallite lexicon **********************
bool Compiler::isKeyword(string s) const {
	vector<string> keywords = {"program", "const", "var", "integer", "boolean",
	"begin", "end", "true", "false", "not", "mod", "div", "and", "or", "read", "write"};
	for (const string &keyword : keywords)
		if (s == keyword)
			return true;
	return false;
}
bool Compiler::isSpecialSymbol(char c) const {
	
	vector<char> specialSymbols = {':', ',', ';', '=', '+', '-', '.', '*', '(', ')', '>', '<'};
	for (char symbol : specialSymbols)
		if (c == symbol)
			return true;
	return false;
}
bool Compiler::isInteger(string s) const {
	if (s.empty()) return false;
    size_t start = 0;
	
    if (s[0] == '+' || s[0] == '-') {
        if (s.length() == 1) 
			return false;
        start = 1;
    }
	
    // Check remaining characters
    for (size_t i = start; i < s.length(); ++i)
        if (!isdigit(s[i])) 
			return false;
    return true;
}
bool Compiler::isBoolean(string s) const {
	if (s.empty()) return false;
	
	if (s == "true" || s == "false")
		return true;
	return false;
}
bool Compiler::isNonKeyId(string s) const {
	if (s.empty()) return false;
	if (!isalpha(s[0]))
        return false;
	for (size_t i = 0; i < s.size(); ++i) {
        char c = s[i];
        if (!(islower(c) || isdigit(c) || c == '_'))
            return false;
	}
	if (isInteger(s))
		return false;
	if (isKeyword(s))
		return false;
	return true;
}
bool Compiler::isLiteral(string s) const {
	if (isInteger(s)) 
		return true;
	if (isBoolean(s)) 
		return true;

	if (s.find("not") == 0) {
		string remainder = s.substr(3);
		if (s[0] == ' ')
			remainder = remainder.substr(1);
        if (isBoolean(remainder))
            return true;
    }
	return false;
}
// ********************** ******************************************** **********************
// ********************** Helper functions for the Pascallite lexicon **********************
string  Compiler :: genInternalName(storeTypes stype) const {
	static int boolCt = 0;
    static int proct = 0;
    static int integerCt = 0;
	string internalName;

	if(stype == INTEGER) {
		internalName =  "I" + to_string(integerCt);
		integerCt++;
	}
	if(stype == BOOLEAN) {
		internalName = "B" + to_string(boolCt);
		boolCt++;
	}
	if(stype == PROG_NAME) {
		internalName = "P" + to_string(proct);
		proct++;
	}
	return internalName;
}

Compiler::Compiler(char **argv) {
	sourceFile.open(argv[1]);
	listingFile.open(argv[2]);
	objectFile.open(argv[3]);
}
Compiler::~Compiler() {
	sourceFile.close();
	listingFile.close();
	objectFile.close();
}

void Compiler::createListingHeader() {
	time_t now = time(NULL);
	listingFile << "STAGE1: " << "Sonpaorong Muchhim  " << ctime(&now) << endl;
	listingFile << "LINE NO.              SOURCE STATEMENT" << endl << endl;
}

void Compiler::parser() {
	nextChar();
	if (nextToken() != "program")
		processError("keyword \"program\" expected");
	prog();
}

void Compiler::createListingTrailer() {
	listingFile << "\nCOMPILATION TERMINATED      " << errorCount << " ERRORS ENCOUNTERED" << endl;
}

void Compiler::processError(string err) {
	errorCount++;
	listingFile << endl << "Error: Line " << lineNo << ": " << err << endl;
	listingFile << "\nCOMPILATION TERMINATED      " << errorCount << " ERROR ENCOUNTERED" << endl;
	exit(EXIT_FAILURE);
}
// ********************** ******************************************** **********************
// ********************** ****** Productions Stage0 & Stage1 ********* **********************
// stage 0, production 1
void Compiler::prog() {
	if (token != "program")
		processError("keyword \"program\" expected");
	progStmt();
	if (token == "const")
		consts();
	if (token == "var")
		vars();
	if (token != "begin")
		processError("keyword \"begin\" expected");
	beginEndStmt();
	if (token[0] != END_OF_FILE)
		processError("no text may follow \"end\"");
}
// stage 0, production 2
void Compiler::progStmt() {
	string x;
	if (token != "program")
		processError("keyword \"program\" expected");
	x = nextToken();
	if (!isNonKeyId(token))
		processError("program name expected");
	if (nextToken() != ";")
		processError("semicolon expected");
	nextToken();
	code("program", x);
	insert(x, PROG_NAME, CONSTANT, x, NO, 0);
}
// stage 0, production 3
void Compiler::consts() {
	if (token != "const")
		processError("keyword \"const\" expected");
	if (!isNonKeyId(nextToken()))
		processError("non-keyword identifier must follow \"const\"");
	constStmts();
}
// stage 0, production 4
void Compiler::vars() {
	if (token != "var")
		processError("keyword \"var\" expected");
	if (!isNonKeyId(nextToken()))
		processError("non-keyword identifier must follow \"var\"");
	varStmts();
}
// stage 0, production 5
void Compiler::beginEndStmt() {
	if (token != "begin")
		processError("keyword \"begin\" expected");
	if(isNonKeyId(nextToken()) || token == "read" || token == "write")
		execStmts();
	if (token != "end" && !isNonKeyId(token) && token != "read" && token != "write")
		processError("keyword \"end\" , NON_KEY_ID, \"read\", or \"write\" expected");
	if (nextToken() != ".")
		processError("period expected");
	nextToken();
	code("end", ".");
}
// stage 0, production 6
void Compiler::constStmts() {
	string x, y;
	if (!isNonKeyId(token))
		processError("non-keyword identifier expected");
	x = token;
	if (nextToken() != "=")
		processError("\"=\" expected");
	y = nextToken();
	if (y != "+" && y != "-" && y != "not" && !isNonKeyId(y) && !isInteger(y) && !isBoolean(y))
		processError("token to right of \"=\" illegal");
	if (y == "+" || y == "-") {
		if (!isInteger(nextToken()))
			processError("integer expected after sign");
		y = y + token;
	}
	if (y == "not") {
		if ((nextToken() != "true" && token != "false") && !isNonKeyId(token)) {
			processError("boolean expected after \"not\"");
		}
		if (token == "true")
			y = "false";
		else
			y = "true";
		if (isNonKeyId(token) && symbolTable.find(token) != symbolTable.end()) {
			string value;
			auto it = symbolTable.find(token);
			value = it-> second.getValue();
			if (value == "-1")
				y = "false";
			if (value == "0")
				y = "true";
		}
	}
	if (nextToken() != ";")
		processError("semicolon expected");
	if (!(whichType(y) == INTEGER || whichType(y) == BOOLEAN))
		processError("data type of token on the right-hand side must be INTEGER or BOOLEAN");
	insert(x, whichType(y), CONSTANT, whichValue(y), YES, 1);
	x = nextToken();
	if (!(x == "begin" || x == "var" || isNonKeyId(x)))
		processError("non-keyword identifier, \"begin\", or \"var\" expected");
	if (isNonKeyId(x))
		constStmts();
}
// stage 0, production 7
void Compiler::varStmts() {
	string x, y, nextOne;
	if (!isNonKeyId(token))
		processError("non-keyword identifier expected");
	x = ids();
	if (token != ":")
		processError("\":\" expected");
	nextOne = nextToken();
	if (!(nextOne == "integer" || nextOne == "boolean"))
		processError("illegal type follows \":\"");
	y = token;
	if (nextToken() != ";")
		processError("semicolon expected");

	if (y == "integer")
		insert(x, INTEGER , VARIABLE, "", YES, 1);
	else
		insert(x, BOOLEAN , VARIABLE, "", YES, 1);
	nextToken();
	if (!(token == "begin" || isNonKeyId(token)))
		processError("non-keyword identifier or \"begin\" expected");
	if (isNonKeyId(token))
		varStmts();
}
// stage 0, production 8
string Compiler::ids() {
	string temp, tempString;
	if (!isNonKeyId(token))
		processError("non-keyword identifier expected");
	tempString = token;
	temp = token;
	if (nextToken() == ",") {
		if (!isNonKeyId(nextToken()))
			processError("non-keyword identifier expected");
		tempString = temp + "," + ids();
	}
	return tempString;
}
// stage 1, production 2
void Compiler::execStmts() {
    
	if (isNonKeyId(token) || token == "read" || token == "write" || token == "begin" ||  token == ";") {
        execStmt();
		execStmts();
	}
	else if(token != "end")
		processError("non-key id, \"read\", \"write\"  End in EXECSTMTS expected ");
}
// stage 1, production 3
void Compiler::execStmt() {       
	
	if(token == "read")
		readStmt();
	else if(token == "write")
		writeStmt();
	else if (isNonKeyId(token))
		assignStmt();
	else if(token == ";")
	  nextToken();
	else if(token == "begin")
		beginEndStmt();
	else
		processError("non-key id, \"read\", \"write\" or \"end\" expected in execStmt");
}
// stage 1, production 4
void Compiler::assignStmt() {
	pushOperand(token);
	if(nextToken() != ":=")
		processError("':=' expected in assignment statement");
	pushOperator(token);
    nextToken();
	if(token != "not" && token != "+" && token != "-" && isNonKeyId(token) == false && token != "(" && isLiteral(token) == false)
		processError("expected non_key_id, integer, \"not\", \"true\", \"false\", '(', '+', or '-'");
	express();
	if(token!= ";")
		processError("one of \"*\", \"and\", \"div\", \"mod\", \")\", \"+\", \"-\", \";\", \"<\", \"<=\", \"<>\", \"=\", \">\", \">=\", or \"or\" expected");
    string opt = popOperator();
	string op2 = popOperand();
	string op3 = popOperand();
	code(opt,op2,op3);
}
// stage 1, production 5
void Compiler::readStmt() {
	string readIn, temp;
    temp = "temp";
    if(token != "read")
        processError("Keyword \"read\" expected" );
    nextToken();
    if(token!= "(")
        processError("'(' expected after \"read\"");
    nextToken();
    if(isNonKeyId(token) == false)
        processError("non keyword identifier expected");
    readIn = ids();

    if(token!=")")
        processError("')' expected are non_key_id in \"read\"");
    nextToken();
    if(token!=";")
        processError("';' expected");
    uint i = 0;
    while(temp != "") {
        temp = "";
        while(readIn[i] != ',' && i < readIn.length()) {
            temp += readIn[i];
            i++;
        }
        i++;
        if(temp != "")
            code("read", temp);
	}
}
// stage 1, production 7
void Compiler::writeStmt() {
	string writeOut, temp;
    temp = "temp";
    if(token != "write")
        processError("Keyword \"write\" expected");
    nextToken();
    if(token!= "(")
        processError("'(' expected after \"write\"");
    nextToken();
    if(isNonKeyId(token) == false)
        processError("non keyword identifier expected");
    writeOut = ids();

    if(token!=")")
        processError("',' or ')' expected after non-keyword identifier");
    nextToken();
    if(token!=";")
        processError("';' expected");
    uint i = 0;
    while(temp != "")
    {
        temp = "";
        while(writeOut[i] != ',' && i < writeOut.length())
        {
            temp += writeOut[i];
            i++;
        }
        i++;
        if(temp != "")
            code("write", temp);
    } 
}
// stage 1, production 9
void Compiler::express() {
	term();
	expresses();
}
// stage 1, production 10
void Compiler::expresses() {
	if(token == "<>" || token == "=" || token == "<=" ||token == ">=" || token == "<" || token == ">") {
        pushOperator(token);
        nextToken();
		term();
        string opt = popOperator();
		string op2 = popOperand();
		string op3 = popOperand();
		code(opt,op2,op3);
		expresses();
	}
}
// stage 1, production 11
void Compiler::term() {
	factor();
	terms(); 
}
// stage 1, production 12
void Compiler::terms() {
	if (token == "-" || token == "+" || token == "or") {
        pushOperator(token);
        nextToken();
		factor();
		string opt = popOperator();
		string op2 = popOperand();
		string op3 = popOperand();
		code(opt,op2,op3);
		terms();
	}
}
// stage 1, production 13
void Compiler::factor() {
	part();
	factors();
}
// stage 1, production 14
void Compiler::factors() {
	if (token == "*" || token == "div" || token == "mod" || token == "and") {
        pushOperator(token);
        nextToken();
		part();
		string opt = popOperator();
		string op2 = popOperand();
		string op3 = popOperand();
		code(opt,op2,op3);     
		factors();
	}
}
// stage 1, production 15
void Compiler::part() {
	if (token == "not") {
		if (nextToken() == "(") {
			nextToken();
			express();
			if(token != ")")
				processError("')' expected");
            string op1 = popOperand();
			code("not", op1);            
            nextToken();
		}
		else if (isBoolean(token)) {
            pushOperand(token == "true"? "false":"true");
			nextToken();
		}
		else if (isNonKeyId(token)) {
            code("not", token);
			nextToken();
		}
		else
			processError("expected '(', boolean, or non-keyword_id");
	}
	else if (token == "+") {
		if (nextToken() == "(") {
			nextToken();
			express();
			if(token != ")")
				processError("')' expected");
            nextToken();
		}
		else if (isInteger(token)) {
            pushOperand(token);
			nextToken();
		}
		else if (isNonKeyId(token)) {
			pushOperand(token);
			nextToken();
		}
		else
			processError("expected '(', integer, or non-keyword_id");
	}
	else if (token == "-") {
		if (nextToken() == "("){
			nextToken();
			express();
			if(token != ")") 
				processError("')' expected");
            string op1 = popOperand();
			code("neg",op1);   
            nextToken();
			
		}
		else if (isInteger(token) && isLiteral(token)) {
            pushOperand('-'+token);
			nextToken();
		}
		else if (isNonKeyId(token)) {
			code("neg", token);
			nextToken();
		}
		else
			processError("expected '(', integer, or non_key_id");
	}
	else if (token == "(") {
        nextToken();
		express();
		if(token != ")")
			processError("')' is expected after EXPRESS");
        nextToken();
	}
	else if (isInteger(token)) {
		pushOperand(token);
		nextToken();
	}
	else if (isBoolean(token)) {
		pushOperand(token);
		nextToken();
	}
	else if (isNonKeyId(token)) {
		pushOperand(token);
		nextToken();
	}
	else
		processError("what's this in part");
}
// ********************** ******************************************** **********************
// ********************** ************ Action Routines *************** **********************
void Compiler::insert(string externalName, storeTypes inType, modes inMode, string inValue, allocation inAlloc, int inUnits) {
	size_t pos = 0;
	const string delimiter = ",";
	string token;
	while ((pos = externalName.find(delimiter)) != string::npos) {
		token = externalName.substr(0, pos);
		externalName.erase(0, pos + delimiter.length());
		token.erase(remove_if(token.begin(), token.end(), ::isspace), token.end());
		if(symbolTable.size()+1 >= 256)
			processError("symbol table overflow --max 256 entries");
		if (symbolTable.find(token) != symbolTable.end())
			processError("multiple name definition");
		else if (isKeyword(token))
			processError("illegal use of keyword");
		else {
			string internalName = isupper(token[0]) ? token : genInternalName(inType);
			symbolTable.insert({token, SymbolTableEntry(internalName, inType, inMode, inValue, inAlloc, inUnits)});
		}
	}
	if (!externalName.empty()) {
		externalName.erase(remove_if(externalName.begin(), externalName.end(), ::isspace), externalName.end());
		string toInsert = inValue;
		if (inType == BOOLEAN)
			toInsert = (inValue == "true" || inValue == "yes") ? "-1" : "0";
		if (symbolTable.find(externalName) != symbolTable.end())
			processError("symbol " + externalName + " is multiply defined");
		else if (isKeyword(externalName))
			processError("illegal use of keyword");
		else {
			string internalName = isupper(token[0]) ? token : genInternalName(inType);
			symbolTable.insert({externalName, SymbolTableEntry(internalName, inType, inMode, inValue, inAlloc, inUnits)});
		}
	}
}
storeTypes Compiler::whichType(string name) {
	storeTypes dataType;
	if (isLiteral(name)) {
		if (isBoolean(name))
			dataType = BOOLEAN;
		else
			dataType = INTEGER;
	}
	else {
		auto it = symbolTable.find(name);
        if (it != symbolTable.end())
            dataType = it->second.getDataType();
		else
			processError("reference to undefined constant");
	}
	return dataType;
}
string Compiler::whichValue(string name) {
	if (isBoolean(name)){
		if (name == "true")
			return "-1";
		else
			return "0";
	}
	else if (isInteger(name))
		return name;
	if (symbolTable.count(name) > 0){
		SymbolTableEntry entry = symbolTable.at(name);
		if (entry.getMode() == CONSTANT)
			return entry.getValue();
	}
	processError("reference to undefined constant");
	return "";
}
void Compiler::code(string op, string operand1, string operand2) {
	if (op == "program")
		emitPrologue(operand1);
	else if (op == "end")
		emitEpilogue(); 
	else if (op == "read") {
		istringstream listOfNames(operand1);
        string name;
        while(getline(listOfNames,name,',')) {
			auto entry = symbolTable.find(name);
			if (entry == symbolTable.end())
				processError("reference to undefined variable in read operation");
			else if(isKeyword(name))
				processError("illegal use of keyword in CODE -> read");
			else if(entry->second.getMode() != VARIABLE)
				processError("can not read to CONSTANT");
			else
				emitReadCode(name);
        }	
	}
	else if (op == "write") {
		istringstream listOfNames(operand1);
		string name;
        while(getline(listOfNames,name,',')) {
			auto entry = symbolTable.find(name);
			if(entry == symbolTable.end())
				processError("reference to undefined variable in write operation");
			else if(isKeyword(name))
				processError("illegal use of keyword in CODE -> write");
			else
				emitWriteCode(name);
        }
	}
	else if (op == "+") // this must be binary '+'
		emitAdditionCode(operand1, operand2);
	else if (op == "-") // this must be binary '-'
		emitSubtractionCode(operand1, operand2);
	else if (op == "neg") // this must be unary '-'
		emitNegationCode(operand1);
	else if (op == "not")
		emitNotCode(operand1);
	else if (op == "*")
		emitMultiplicationCode(operand1, operand2);// op2 * op1
	else if (op == "div")
		emitDivisionCode(operand1, operand2);// op2 / op1
	else if (op == "mod")
		emitModuloCode(operand1, operand2);// op2 % op1
	else if (op == "and")
		emitAndCode(operand1, operand2);// op2 && op1
	else if (op == "or")
		emitOrCode(operand1, operand2);// op2 || op1
	else if (op == "=")
		emitEqualityCode(operand1, operand2);// op2 == op1
	else if (op == ":=")
		emitAssignCode(operand1, operand2);// op2 = op1
	else if (op == "<>")
		emitInequalityCode(operand1, operand2);// op2 != op1
	else if (op == "<")
		emitLessThanCode(operand1, operand2);// op2 <  op1
	else if (op == "<=")
		emitLessThanOrEqualToCode(operand1, operand2);// op2 <= op1
	else if (op == ">")
		emitGreaterThanCode(operand1, operand2);// op2 >  op1
	else if (op == ">=")
		emitGreaterThanOrEqualToCode(operand1, operand2);// op2 >= op1
	else
		processError("compiler error since function code should not be called with illegal arguements");
}
void Compiler::pushOperator(string op) {
	operatorStk.push(op);
}
string Compiler::popOperator() {
	if (operatorStk.empty() == false){
		string topOfStack = operatorStk.top();
		operatorStk.pop();
		return topOfStack;
	}
	else
        processError("compiler error; operator stack underflow");
	
    return "";
}
void Compiler :: pushOperand(string operand) {
	if(isLiteral(operand) && symbolTable.count(operand) == 0) {
		if(operand == "true")
			insert("TRUE", BOOLEAN, CONSTANT, operand, YES, 1);
		else if(operand == "false")
			insert("FALSE", BOOLEAN, CONSTANT, operand, YES, 1);
		else
			insert(operand, whichType(operand), CONSTANT, operand, YES, 1);
	}
	else if (!(isLiteral(operand)) && symbolTable.count(operand) == 0)
		processError("reference to undefined symbol " + operand);
	operandStk.push(operand);
}
string Compiler :: popOperand() {
	if (!operandStk.empty()) {
		string topElement = operandStk.top();
		operandStk.pop();
		return topElement;
	} 
	else
		processError("compiler error; operand stack underflow");
	return "";
}
// ********************** ******************************************** **********************
// ********************** ************ Emit  Functions *************** **********************
void Compiler::emit(string label, string instruction, string operands, string comment) {
	objectFile << left;
	objectFile << setw(8) << label;
	objectFile << setw(8) << instruction;
	objectFile << setw(24) << operands;
	objectFile << comment << endl;
}
void Compiler::emitPrologue(string progName, string operand2) {
	time_t now = time (NULL);
	objectFile << "; " << "Sonpaorong Muchhim " << ctime(&now);
	objectFile << "%INCLUDE \"Along32.inc\"" << endl;
	objectFile << "%INCLUDE \"Macros_Along.inc\"" << endl << endl;
	emit("SECTION", ".text");
	emit("global", "_start", "", "; program " + progName);
	objectFile << endl;
	emit("_start:");
}
void Compiler::emitEpilogue(string operand1, string operand2){
	emit("", "Exit", "{0}");
	emitStorage();
}
void Compiler::emitStorage() {
	objectFile << endl;
	emit("SECTION", ".data");
	for (const auto &entry : symbolTable){
		if (entry.second.getAlloc() == YES && entry.second.getMode() == CONSTANT) {
			string asmData = "dd";
			string comment = "; " + entry.first;
			if (entry.first == "yes")
				emit(entry.second.getInternalName(), asmData, "-1", comment);
			else
				emit(entry.second.getInternalName(), asmData, entry.second.getValue(), comment);
		}
	}
	objectFile << endl;
	emit("SECTION", ".bss");

	for (const auto &entry : symbolTable) {
		if (entry.second.getAlloc() == YES && entry.second.getMode() == VARIABLE) {
			string asmData = "resd";
			string comment = "; " + entry.first;
			string compare = entry.first;
			if (compare[0] == 'T')
				emit(entry.first, asmData, "1", comment);
			else
				emit(entry.second.getInternalName(), asmData, "1", comment);
		}
	}
}
void Compiler::emitReadCode(string operand, string) {
	istringstream listOfNames(operand);
    string name;
    while(getline(listOfNames,name,',')) {
		auto entry = symbolTable.find(name);
		if (entry == symbolTable.end())
			processError("reference to undefined variable in read: " + operand);
		else if (entry->second.getDataType() != INTEGER)
			processError("can't read variables of this type");
		else if (entry->second.getMode() != VARIABLE)
			processError("attempting to read to a read-only location" + operand);

		string internalName = entry->second.getInternalName();
	
		emit("", "call", "ReadInt", "; read int; value placed in eax");
		emit("", "mov", "[" + internalName + "],eax", "; store eax at " + name);
		contentsOfAReg = name;
	}
}
void Compiler::emitWriteCode(string operand, string) {
	istringstream listOfNames(operand);
    string name;
    while(getline(listOfNames,name,',')) {
		auto entry = symbolTable.find(name);
		if (entry == symbolTable.end())
			processError("reference to undefined variable in write: " + operand);
		if (name != contentsOfAReg) {
			string internalName = entry->second.getInternalName();
			emit("", "mov", "eax,[" + internalName + "]", "; load " + name + " in eax");
			contentsOfAReg = name;
		}
		if (entry->second.getDataType() == INTEGER || entry->second.getDataType() == BOOLEAN)
			emit("", "call", "WriteInt", "; write int in eax to standard out");
		emit("", "call", "Crlf", "; write \\r\\n to standard out");
	}
}
void Compiler::emitAssignCode(string operand1, string operand2) {
	
	if (whichType(operand1) != whichType(operand2)) 
		processError("incompatible types for operator ':='");
	if (symbolTable.at(operand2).getMode() != VARIABLE)
		processError("symbol on left-hand side of assignment must have a storage mode of VARIABLE");
	if (operand1 == operand2) {return;}
	if (operand1 != contentsOfAReg) {
		string intName = symbolTable.at(operand1).getInternalName();
		emit("", "mov", "eax,[" + intName + "]", "; AReg = " + operand1);
	}
	string intName = symbolTable.at(operand2).getInternalName();
	emit("", "mov", "[" + intName + "],eax", "; " + operand2 + " = AReg");
	contentsOfAReg = operand2;
	if (isTemporary(operand1))
		freeTemp();
}
void Compiler::emitAdditionCode(string operand1, string operand2) {
	
	if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
		processError("binary '+' requires integer operands");
	
	if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		if (isTemporary(contentsOfAReg))
			emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		else
			emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2)
		contentsOfAReg = "";
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		string intName = symbolTable.at(operand2).getInternalName();
		emit("", "mov", "eax,[" + intName + "]", "; AReg = " + operand2);
	}
	if(contentsOfAReg == operand1) {
		if (isTemporary(operand2))
			emit("","add", "eax,[" + operand2 + "]", "; AReg = " + operand1 + " + " + operand2);
		else
			emit("","add", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " + " + operand2);
	}
	else {
	    if (isTemporary(operand1))
			emit("","add", "eax,[" + operand1 + "]", "; AReg = " + operand2 + " + " + operand1);
		else
			emit("","add", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " + " + operand1);
	}
	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	pushOperand(contentsOfAReg);
}
void Compiler::emitSubtractionCode(string operand1, string operand2) {
	
	if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
		processError("binary '-' requires integer operands ");
	if(isTemporary(contentsOfAReg)  && contentsOfAReg != operand2){
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if(contentsOfAReg != operand2){
		if(isTemporary(operand2))
			emit("", "mov","eax,[" + operand2 + "]", "; AReg = " + operand2);
		else
			emit("", "mov","eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}
	if (isTemporary(operand1))
		emit("", "sub", "eax,[" + operand1 + "]", "; AReg = " + operand2 + " - " +operand1 );
	else
		emit("", "sub", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " - " +operand1 );
	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	pushOperand(contentsOfAReg);
}
void Compiler::emitMultiplicationCode(string operand1, string operand2) {
	
	if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
		processError("binary '*' requires integer operands");
	if (isTemporary(contentsOfAReg) == true && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg" );
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if (isTemporary(contentsOfAReg) == false && (contentsOfAReg != operand1 && contentsOfAReg != operand2))
		contentsOfAReg = "";
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}

	if(contentsOfAReg == operand1) {
		if (isTemporary(operand2))
			emit("", "imul", "dword [" + operand2 + "]", "; AReg = " + operand1 + " * " + operand2);
		else 
			emit("", "imul", "dword [" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " * " + operand2);
	}
	else{
		if (isTemporary(operand1))
			emit("", "imul", "dword [" + operand1 + "]", "; AReg = " + operand2 + " * " + operand1);
		else
			emit("", "imul", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " * " + operand1);
	}
	
	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	pushOperand(contentsOfAReg);
}
void Compiler::emitDivisionCode (string operand1, string operand2) {
	
	if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
		processError("binary 'div' requires integer operands");
	if (isTemporary(contentsOfAReg) == true && contentsOfAReg != operand2) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand2)
		contentsOfAReg = "";
	if (contentsOfAReg != operand2)
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
	
	emit("","cdq","","; sign extend dividend from eax to edx:eax");
	if(isTemporary(operand1))
		emit("", "idiv", "dword [" + operand1 + "]", "; AReg = " + operand2 + " div " + operand1);
	else 
		emit("", "idiv", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " div " + operand1);
	
	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	pushOperand(contentsOfAReg);
}
void Compiler::emitModuloCode(string operand1, string operand2) {
	
	if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
		processError("binary 'mod' requires integer operands");
	if (isTemporary(contentsOfAReg) == true && contentsOfAReg != operand2) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; fill this in");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if (isTemporary(contentsOfAReg) == false && contentsOfAReg != operand2)
		contentsOfAReg = "";
	if (contentsOfAReg != operand2)
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
	
	emit("","cdq","","; sign extend dividend from eax to edx:eax");
	emit("", "idiv", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " div " + operand1);
	emit("", "xchg", "eax,edx", "; exchange quotient and remainder");

	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	pushOperand(contentsOfAReg);
}
void Compiler::emitNegationCode (string operand1, string) {
	
	if (whichType(operand1) != INTEGER)
		processError("unary '-' requires an integer operand");
	if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if (isTemporary(contentsOfAReg) == false && contentsOfAReg != operand1)
		contentsOfAReg = "";
	if(contentsOfAReg != operand1){
		emit("", "mov", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand1);
		contentsOfAReg = operand1;
	}
	emit("", "neg ", "eax", "; AReg = -AReg");

	if(isTemporary(operand1))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	pushOperand(contentsOfAReg);
}
void Compiler::emitNotCode(string operand1, string) {
	
	if (whichType(operand1) != BOOLEAN)
		processError("illegal type");
	if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1) {
		emit("", "mov","[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if (isTemporary(contentsOfAReg) == false && contentsOfAReg != operand1)
		contentsOfAReg = "";
	if(contentsOfAReg != operand1){
		emit("", "mov", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand1);
		contentsOfAReg = operand1;
	}
	emit("", "not", "eax", "; AReg = !AReg");

	if(isTemporary(operand1))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg); 
}
void Compiler::emitAndCode(string operand1, string operand2) {
	
	if(whichType(operand1) != BOOLEAN || whichType(operand2) != BOOLEAN)
		processError("binary 'and' requires boolean operands");
	if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov","[" + contentsOfAReg + "],eax", "; deassign AReg" );
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if(contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}
	if (contentsOfAReg == operand1) {
		if (isTemporary(operand2))
			emit("", "and", "eax,[" + operand2 + "]", "; AReg = " + operand1 + " and " + operand2);
		else
			emit("", "and", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " and " + operand2);
	}
	else if (contentsOfAReg == operand2) {
		if (isTemporary(operand2))
			emit("", "and", "eax,[" + operand1 + "]", "; AReg = " + operand2 + " and " + operand1);
		else
			emit("", "and", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " and " + operand1);
	}

	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);
}
void Compiler::emitOrCode(string operand1, string operand2) {
	if (whichType(operand1) != BOOLEAN || whichType(operand2) != BOOLEAN)
		processError("illegal type");
	if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if (isTemporary(contentsOfAReg) == false && contentsOfAReg != operand1 && contentsOfAReg != operand2)
		contentsOfAReg = "";
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}
	if (contentsOfAReg == operand1) {
		if (isTemporary(operand2))
			emit("", "or", "eax,[" + operand2 + "]", "; AReg = " + operand1 + " or " + operand2);
		else
			emit("", "or", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " or " + operand2);
	}
	else if (contentsOfAReg == operand2) {
		if (isTemporary(operand2))
			emit("", "or", "eax,[" + operand1 + "]", "; AReg = " + operand2 + " or " + operand1);
		else
			emit("", "or", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " or " + operand1);
	}
	
	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg); 
}
void Compiler::emitEqualityCode(string operand1, string operand2) {
	
	string label1, label2;
	label1 = getLabel();
	label2 = getLabel();

	if (whichType(operand1) != whichType(operand2))
		processError("binary '=' requires operands of the same type");
	if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg"); 
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if (isTemporary(contentsOfAReg) == false && (contentsOfAReg != operand1 && contentsOfAReg != operand2))
		contentsOfAReg = "";
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}
	if (contentsOfAReg == operand2)
		emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]","; compare " + operand2 + " and " + operand1);
	else
		emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]","; compare " + operand1 + " and " + operand2);
	
	emit("", "je", label1, "; if " + operand2 + " = " + operand1 + " then jump to set eax to TRUE" );
	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
	emit("", "jmp", label2, "; unconditionally jump");
	emit(label1 + ":", "", "", "");
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");
	emit(label2 + ":", "", "", "");

	if (symbolTable.count("true") == 0)
		symbolTable.insert({"true", SymbolTableEntry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1)});
	if (symbolTable.count("false") == 0)
		symbolTable.insert({"false", SymbolTableEntry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1)});

	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg); 
	
}
void Compiler::emitInequalityCode(string operand1, string operand2) {
	
	string label1, label2;
	label1 = getLabel();
	label2 = getLabel();

	if (whichType(operand1) != whichType(operand2))
		processError("binary '<>' requires operands of the same type");

	if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if (isTemporary(contentsOfAReg) == false && (contentsOfAReg != operand1 && contentsOfAReg != operand2))
		contentsOfAReg = "";
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2)
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
	
	if (contentsOfAReg == operand1)
		emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]","; compare " + operand1 + " and " + operand2);
	else
		emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]","; compare " + operand2 + " and " + operand1);
	
	emit("", "jne", label1, "; if " + operand2 + " <> " + operand1 +" then jump to set eax to TRUE");
	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
	emit("", "jmp", label2, "; unconditionally jump");
	emit(label1 + ":", "", "", "");
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");
	emit(label2 + ":", "", "", "");

	if (symbolTable.count("true") == 0)
		symbolTable.insert({ "true", SymbolTableEntry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1) });
	if (symbolTable.count("false") == 0)
		symbolTable.insert({ "false", SymbolTableEntry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1) });
	
	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);
}
void Compiler::emitLessThanCode(string operand1, string operand2) {
	
	string label1 = getLabel();
	string label2 = getLabel();

	if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
		processError("binary '<' requires integer operands");
	
	if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg" );
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if(contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}
	emit("", "cmp ", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
	emit("", "jl ", label1, "; if " + operand2 + " < " + operand1 + " then jump to set eax to TRUE");
	emit("", "mov ", "eax,[FALSE]", "; else set eax to FALSE");
	emit("", "jmp ", label2, "; unconditionally jump");
	emit(label1 + ":", "", "" ,"");
	emit("", "mov ", "eax,[TRUE]", "; set eax to TRUE");
	emit(label2 + ":", "", "" ,"");
	
	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);
}
void Compiler::emitLessThanOrEqualToCode(string operand1, string operand2) {
	
	string label1 = getLabel();
	string label2 = getLabel();

	if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
		processError("binary '<=' requires integer operands");
	
	if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg" );
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if(contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}
	emit("", "cmp ", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
	emit("", "jle ", label1, "; if " + operand2 + " <= " +operand1 + " then jump to set eax to TRUE" );
	emit("", "mov ", "eax,[FALSE]", "; else set eax to FALSE");
	emit("", "jmp ", label2, "; unconditionally jump");
	emit(label1 + ":", "", "" ,"");
	emit("", "mov ", "eax,[TRUE]", "; set eax to TRUE");
	emit(label2 + ":", "", "" ,"");
		
	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);
}
void Compiler::emitGreaterThanCode(string operand1, string operand2) {
	
	string label1, label2;
	label1 = getLabel();
	label2 = getLabel();
	
	if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
		processError("binary '>' requires integer operands");
	
	if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	
	if (isTemporary(contentsOfAReg) == false && (contentsOfAReg != operand1 && contentsOfAReg != operand2))
		contentsOfAReg = "";
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2)
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
	emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]","; compare " + operand2 + " and " + operand1);
	emit("", "jg", label1, "; if " + operand2 + " > " + operand1 + " then jump to set eax to TRUE");
	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
	emit("", "jmp", label2, "; unconditionally jump");
	emit(label1 + ":", "", "", "");
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");
	emit(label2 + ":", "", "", "");
	
	if (symbolTable.count("true") == 0)
		symbolTable.insert({ "true", SymbolTableEntry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1) });
	if (symbolTable.count("false") == 0)
		symbolTable.insert({ "false", SymbolTableEntry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1) });
	
	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);
}
void Compiler::emitGreaterThanOrEqualToCode(string operand1, string operand2) {
	
	string label1, label2;
	label1 = getLabel();
	label2 = getLabel();
	
	if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
		processError("binary '>=' requires integer operands");

	if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	
	if (isTemporary(contentsOfAReg) == false && (contentsOfAReg != operand1 && contentsOfAReg != operand2))
		contentsOfAReg = "";
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}
	emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]","; compare " + operand2 + " and " + operand1);
	emit("", "jge", label1, "; if " + operand2 + " >= " + operand1 + " then jump to set eax to TRUE");
	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
	emit("", "jmp", label2, "; unconditionally jump");
	emit(label1 + ":", "", "", "");
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");
	emit(label2 + ":", "", "", "");
	
	if (symbolTable.count("true") == 0)
		symbolTable.insert({ "true", SymbolTableEntry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1) });
	if (symbolTable.count("false") == 0)
		symbolTable.insert({ "false", SymbolTableEntry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1) });
	
	if(isTemporary(operand1))
		freeTemp();
	if(isTemporary(operand2))
		freeTemp();
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);
}
// ********************** **************** **********************
// ********************** Other routines **********************
void Compiler :: freeTemp() {
	currentTempNo--;
	if(currentTempNo < -1)
		processError("compiler error, currentTempNo should be ≥ –1");
}
string Compiler :: getTemp() {
	string temp;
	currentTempNo++;
	temp = "T" + to_string(currentTempNo); 
	if (currentTempNo > maxTempNo) {
		
		insert(temp, UNKNOWN, VARIABLE, "", NO, 1);
		maxTempNo++;
	}
	return temp;
}
string Compiler :: getLabel() {
	static int labelCount = -1;
	string currentLabel;
	labelCount++; 
	currentLabel = ".L" + to_string(labelCount);
	return currentLabel; 
}  
bool Compiler :: isTemporary(string s) const {
	if(s[0] == 'T')
		return true;
	else
		return false;
}
// ********************** **************** **********************
// ********************** Lexical routines **********************
string Compiler::nextToken() {
	token = "";
	while (token == "") {
		if (ch == '{') {
			while (nextChar() != END_OF_FILE && ch != '}') {}
			if (ch == END_OF_FILE)
				processError("unexpected end of file");
			else
				nextChar();
		}
		else if (ch == '}')
			processError("'}' cannot begin a token");
		else if (isspace(ch))
			nextChar();
		else if (isSpecialSymbol(ch)) {
			if(ch == ':'){
				token += ch;
				nextChar();
				if(ch == '='){
					token += ch;
					nextChar();
				}
			}
			else if(ch == '<'){
				token += ch;
				nextChar();
				if(ch == '='){
					token += ch;
					nextChar();
				}
				else if(ch == '>'){
					token += ch;
					nextChar();
				}
			}
			else if(ch == '>'){
				token += ch;
				nextChar();
				if(ch == '='){
					token += ch;
					nextChar();
				}
			}
			else {
				token = ch;
				nextChar();
			}
		}
		else if (islower(ch)) {
			token = ch;
			while ((islower(nextChar()) || isdigit(ch) || ch == '_') && ch != END_OF_FILE) {
				if (token.back() == '_' && ch == '_')
					processError("encountered consecutive underscores");
				token += ch;
			}
			if (token.back() == '_')
				processError("'_' cannot end a token");
			if (ch == END_OF_FILE)
				processError("unexpected end of file");
			if(token.length()>15)
				token = token.substr(0,15);
		}
		else if (isdigit(ch)) {
			token += ch;
			while (isdigit(nextChar()) && ch != END_OF_FILE)
				token += ch;
			if (!isInteger(token))
				processError("no real numbers are allowed");
			if (ch == END_OF_FILE)
				processError("unexpected end of file");
		}
		else if (ch == END_OF_FILE) {
			token = ch;
		}
		else
			processError("illegal symbol from here? ");
	}
	return token;
}
char Compiler::nextChar() {
	sourceFile.get(ch);
	static char preChar = '\n';
	if (sourceFile.eof()) {
		ch = END_OF_FILE;
		return ch;
	}
	if (ch != '$') {
		if (preChar == '\n')
			listingFile << setw(5) << ++lineNo << '|';
		listingFile << ch;
	}
	else
		listingFile << ch;
	preChar = ch;
	return ch;
}
// ********************** **************** **********************