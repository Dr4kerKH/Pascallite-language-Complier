// Name: Sonpaorong Muchhim
// CS 4301
// Complier Project: Stage0

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <ctime>
#include <cctype>
#include <stage0.h>
#include <algorithm>
#include <cstdio>
#include <iomanip>
using namespace std;

bool Compiler::isKeyword(string s) const {
	vector<string> keywords = {"program", "const", "var", "integer", "boolean", "begin", "end", "true", "false", "not"};
	for (const string &keyword : keywords)
		if (s == keyword)
			return true;
	return false;
}

bool Compiler::isSpecialSymbol(char c) const {
	const string specialSymbols = ":,;=+-.";
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
	// Extract string after "not" and check if remainder is a valid BOOLEAN
	if (s.find("not") == 0) {
		string remainder = s.substr(3);
		if (s[0] == ' ')
			remainder = remainder.substr(1);
        if (isBoolean(remainder))
            return true;
    }
	return false;
}

string Compiler::genInternalName(storeTypes stype) const {
	static int booleans = 0;
	static int integers = 0;
	string internalName;
	switch (stype) {
		case PROG_NAME:
			internalName = "P0";
			break;
		case INTEGER:
			internalName = "I" + to_string(integers);
			integers++;
			break;
		case BOOLEAN:
			internalName = "B" + to_string(booleans);
			booleans++;
			break;
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
	listingFile << "STAGE0: " << "Sonpaorong Muchhim  " << ctime(&now) << endl;
	listingFile << "LINE NO.              SOURCE STATEMENT" << endl << endl;
}

void Compiler::parser() {
	nextChar();
	if (nextToken() != "program")
		processError("keyword \"program\" expected");
	// Calling 1st production
	prog();
}

void Compiler::createListingTrailer() {
	listingFile << "\nCOMPILATION TERMINATED      " << errorCount << " ERRORS ENCOUNTERED" << endl;
}

void Compiler::processError(string err) {
	errorCount++;
	listingFile << endl << endl << "Error: Line " << lineNo << ": " << err << endl;
	listingFile << "\nCOMPILATION TERMINATED      " << errorCount << " ERROR ENCOUNTERED" << endl;
	exit(EXIT_FAILURE);
}
// prog() - production 1
void Compiler::prog() {
	if (token != "program")
		processError("keyword \"program\" expected");
	// Calling 2nd production
	progStmt();
	// Calling 3rd production
	if (token == "const")
		consts();
	// Calling 4th production
	if (token == "var")
		vars();
	if (token != "begin")
		processError("keyword \"begin\" expected");
	// Calling 5th production
	beginEndStmt();
	if (token[0] != END_OF_FILE)
		processError("no text may follow \"end\"");
}
// progStmt() - production 2
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
// consts() - production 3
void Compiler::consts() {
	if (token != "const")
		processError("keyword \"const\" expected");
	if (!isNonKeyId(nextToken()))
		processError("non-keyword identifier must follow \"const\"");
	// Calling 6th production
	constStmts();
}
// vars() - production 4
void Compiler::vars() {
	if (token != "var")
		processError("keyword \"var\" expected");
	if (!isNonKeyId(nextToken()))
			processError("non-keyword identifier must follow \"var\"");
	varStmts();
}
// beginEndStmt() - production 5
void Compiler::beginEndStmt() {
	if (token != "begin")
		processError("keyword \"begin\" expected");
	if (nextToken() != "end")
		processError("keyword \"end\" expected");
	if (nextToken() != ".")
		processError("period expected");
	nextToken();
	code("end", ".");
}
// constStmts() - production 6
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
		if (nextToken() != "true" && token != "false")
			processError("boolean expected after \"not\"");
		if (token == "true")
			y = "false";
		else
			y = "true";
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
// varStmts() - production 7
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
// ids() - production 8
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

void Compiler::insert(string externalName, storeTypes inType, modes inMode, string inValue, allocation inAlloc, int inUnits) {
	size_t pos = 0;
	const string delimiter = ",";
	string token;
	while ((pos = externalName.find(delimiter)) != string::npos) {
		token = externalName.substr(0, pos);
		externalName.erase(0, pos + delimiter.length());
		token.erase(remove_if(token.begin(), token.end(), ::isspace), token.end());
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
	else
		processError("compiler error since function code should not be called with illegal arguements");
}

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
	// Iterate through the symbol table and emit storage for variables
	for (const auto &entry : symbolTable) {
		if (entry.second.getAlloc() == YES && entry.second.getMode() == VARIABLE) {
			string asmData = "resd";
			string comment = "; " + entry.first;
			emit(entry.second.getInternalName(), asmData, "1", comment);
		}
	}
}

string Compiler::nextToken() {
	token = "";
	while (token == "") {
		if (ch == '{') {
			while (nextChar() != END_OF_FILE && ch != '}') {
			}
			if (ch == END_OF_FILE) {
				processError("unexpected end of file");
			}
			else
				nextChar();
		}
		else if (ch == '}') {
			processError("'}' cannot begin a token");
		}
		else if (isspace(ch))
			nextChar();
		else if (isSpecialSymbol(ch)) {
			token = ch;
			nextChar();
		}
		else if (islower(ch)) {
			token = ch;
			while ((islower(nextChar()) || isdigit(ch) || ch == '_') && ch != END_OF_FILE) {
				if (token.back() == '_' && ch == '_') {
					processError("encountered consecutive underscores");
				}
				token += ch;
			}
			if (token.back() == '_') {
				processError("'_' cannot end a token");
			}
			if (ch == END_OF_FILE) {
				processError("unexpected end of file");
			}
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
		else if (ch == END_OF_FILE)
			token = ch;
		else
			processError("illegal symbol");
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
	else {
		if (preChar == '\n')
			listingFile << setw(5) << ++lineNo << '|';
		listingFile << ch;
	}
	preChar = ch;
	return ch;
}