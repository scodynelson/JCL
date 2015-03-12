package jcl.compiler.real.icg;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Stack;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.icg.generator.specialoperator.TagbodyLabel;
import jcl.lists.ListStruct;

public class JavaClassBuilder {

	// this is the current binding environment. It always matches the value
	// on top of the binding stack
	private Environment bindingEnvironment;
	// Whenever a binding environment is encountered, it is pushed on the stack and
	// bindingEnvironment is set to the new environment. When that binding is no
	// longer in force, the stack is popped and the value of bindingEnvironment is
	// set to the new top of stack
	private Stack<Environment> bindingStack;
	// make a stack of current class names
	private Stack<String> classNames;
	private NewEmitter emitter;
	private boolean allowMultipleValues;
	private Stack<Stack<TagbodyLabel>> tagbodyStack;
	private ListStruct sourceFile;
	private int LineNumber;

	private int tagCounter;

	private boolean MacroLambda;

	private final List<ClassDef> classes = Collections.synchronizedList(new ArrayList<>());
	private final Stack<ClassDef> classStack = new Stack<>();
	private ClassDef currentClass;

	public JavaClassBuilder() {
		MacroLambda = false;
		emitter = new NewEmitter();
		bindingEnvironment = Environment.NULL;
		bindingStack = new Stack<>();
		bindingStack.push(Environment.NULL);
		classNames = new Stack<>();
		tagCounter = 0;
		allowMultipleValues = false;
		tagbodyStack = new Stack<>();
	}

	public NewEmitter getEmitter() {
		return emitter;
	}

	public void setEmitter(final NewEmitter emitter) {
		this.emitter = emitter;
	}

	public Environment getBindingEnvironment() {
		return bindingEnvironment;
	}

	public void setBindingEnvironment(final Environment bindingEnvironment) {
		this.bindingEnvironment = bindingEnvironment;
	}

	public Stack<Environment> getBindingStack() {
		return bindingStack;
	}

	public void setBindingStack(final Stack<Environment> bindingStack) {
		this.bindingStack = bindingStack;
	}

	public Stack<String> getClassNames() {
		return classNames;
	}

	public void setClassNames(final Stack<String> classNames) {
		this.classNames = classNames;
	}

	public boolean isAllowMultipleValues() {
		return allowMultipleValues;
	}

	public void setAllowMultipleValues(final boolean allowMultipleValues) {
		this.allowMultipleValues = allowMultipleValues;
	}

	public Stack<Stack<TagbodyLabel>> getTagbodyStack() {
		return tagbodyStack;
	}

	public void setTagbodyStack(final Stack<Stack<TagbodyLabel>> tagbodyStack) {
		this.tagbodyStack = tagbodyStack;
	}

	public ListStruct getSourceFile() {
		return sourceFile;
	}

	public void setSourceFile(final ListStruct sourceFile) {
		this.sourceFile = sourceFile;
	}

	public int getLineNumber() {
		return LineNumber;
	}

	public void setLineNumber(final int lineNumber) {
		LineNumber = lineNumber;
	}

	public int getTagCounter() {
		return tagCounter;
	}

	public void setTagCounter(final int tagCounter) {
		this.tagCounter = tagCounter;
	}

	/**
	 * the rest Lambda generated will be for a macro
	 */
	public boolean isMacroLambda() {
		return MacroLambda;
	}

	public void setMacroLambda(final boolean macroLambda) {
		MacroLambda = macroLambda;
	}

	public List<ClassDef> getClasses() {
		return classes;
	}

	public Stack<ClassDef> getClassStack() {
		return classStack;
	}

	public ClassDef getCurrentClass() {
		return currentClass;
	}

	public void setCurrentClass(final ClassDef currentClass) {
		this.currentClass = currentClass;
	}
}