package jcl.compiler.real.icg;

import java.util.Deque;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.ConcurrentLinkedDeque;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.icg.generator.specialoperator.TagbodyLabel;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;

public class JavaClassBuilder {

	private final Deque<ClassDef> classes = new ConcurrentLinkedDeque<>();

	private final Stack<ClassDef> classStack = new Stack<>();

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

	private Stack<Set<TagbodyLabel>> tagbodyLabelStack;

	private ListStruct sourceFile;

	private int LineNumber;

	private int tagCounter;

	private boolean MacroLambda;

	private ClassDef currentClass;

	private boolean acceptsMultipleValues;

	final Map<SymbolStruct<?>, Integer> fletFunctionStoresToBind = new HashMap<>();

	public JavaClassBuilder() {
		MacroLambda = false;
		emitter = new NewEmitter();
		bindingEnvironment = Environment.NULL;
		bindingStack = new Stack<>();
		bindingStack.push(Environment.NULL);
		classNames = new Stack<>();
		tagCounter = 0;
		allowMultipleValues = false;
		tagbodyLabelStack = new Stack<>();
		acceptsMultipleValues = false;
	}

	public NewEmitter getEmitter() {
		return emitter;
	}

	public void setEmitter(final NewEmitter emitter) {
		this.emitter = emitter;
	}

	public Environment getBindingEnvironment() {
		return bindingStack.peek();
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

	public Stack<Set<TagbodyLabel>> getTagbodyLabelStack() {
		return tagbodyLabelStack;
	}

	public void setTagbodyLabelStack(final Stack<Set<TagbodyLabel>> tagbodyLabelStack) {
		this.tagbodyLabelStack = tagbodyLabelStack;
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

	public int getNextTagbodyTagIndex() {
		return tagCounter++;
	}

	public boolean isMacroLambda() {
		return MacroLambda;
	}

	public void setMacroLambda(final boolean macroLambda) {
		MacroLambda = macroLambda;
	}

	public Deque<ClassDef> getClasses() {
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

	public boolean isAcceptsMultipleValues() {
		return acceptsMultipleValues;
	}

	public void setAcceptsMultipleValues(final boolean acceptsMultipleValues) {
		this.acceptsMultipleValues = acceptsMultipleValues;
	}

	public int getTagCounter() {
		return tagCounter;
	}

	public Map<SymbolStruct<?>, Integer> getFletFunctionStoresToBind() {
		return fletFunctionStoresToBind;
	}
}
