package jcl.compiler.real.icg;

import java.util.Deque;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.ConcurrentLinkedDeque;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.icg.generator.specialoperator.TagbodyLabel;

public class GeneratorState {

	private final Deque<JavaClassBuilder> classes = new ConcurrentLinkedDeque<>();

	private final Stack<JavaClassBuilder> classStack = new Stack<>();

	private final Stack<JavaMethodBuilder> methodBuilderStack = new Stack<>();

	// Whenever a binding environment is encountered, it is pushed on the stack and
	// bindingEnvironment is set to the new environment. When that binding is no
	// longer in force, the stack is popped and the value of bindingEnvironment is
	// set to the new top of stack
	private Stack<Environment> bindingStack;

	private Stack<Set<TagbodyLabel>> tagbodyLabelStack;

	private int tagCounter;

	private JavaClassBuilder currentClass;

	public GeneratorState() {
		bindingStack = new Stack<>();
		bindingStack.push(Environment.NULL);
		tagCounter = 0;
		tagbodyLabelStack = new Stack<>();
	}

	public Stack<Environment> getBindingStack() {
		return bindingStack;
	}

	public void setBindingStack(final Stack<Environment> bindingStack) {
		this.bindingStack = bindingStack;
	}

	public Stack<Set<TagbodyLabel>> getTagbodyLabelStack() {
		return tagbodyLabelStack;
	}

	public int getNextTagbodyTagIndex() {
		return tagCounter++;
	}

	public Deque<JavaClassBuilder> getClasses() {
		return classes;
	}

	public Stack<JavaClassBuilder> getClassStack() {
		return classStack;
	}

	public Stack<JavaMethodBuilder> getMethodBuilderStack() {
		return methodBuilderStack;
	}

	public JavaMethodBuilder getCurrentMethodBuilder() {
		if (methodBuilderStack.empty()) {
			return null;
		}
		return methodBuilderStack.peek();
	}

	public JavaClassBuilder getCurrentClass() {
		return currentClass;
	}

	public void setCurrentClass(final JavaClassBuilder currentClass) {
		this.currentClass = currentClass;
	}
}
