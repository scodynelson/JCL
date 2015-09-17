package jcl.compiler.real.icg;

import java.util.Deque;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.ConcurrentLinkedDeque;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import org.objectweb.asm.Label;

public class GeneratorState {

	private final Deque<JavaClassBuilder> classes = new ConcurrentLinkedDeque<>();

	private final Stack<JavaClassBuilder> classStack = new Stack<>();

	private final Stack<JavaMethodBuilder> methodBuilderStack = new Stack<>();

	// Whenever a binding environment is encountered, it is pushed on the stack and
	// bindingEnvironment is set to the new environment. When that binding is no
	// longer in force, the stack is popped and the value of bindingEnvironment is
	// set to the new top of stack
	private final Stack<Environment> bindingStack;

	private final Stack<Set<TagbodyLabel>> tagbodyLabelStack;

	private int tagCounter;

	private JavaClassBuilder currentClass;

	public GeneratorState() {
		bindingStack = new Stack<>();
		bindingStack.push(Environment.NULL);
		tagCounter = 0;
		tagbodyLabelStack = new Stack<>();
		currentClass = null;
	}

	public Stack<Environment> getBindingStack() {
		return bindingStack;
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

	public static class TagbodyLabel {

		private final GoStruct<?> tag;

		private final int index;

		private final Label label;

		public TagbodyLabel(final GoStruct<?> tag, final int index, final Label label) {
			this.tag = tag;
			this.index = index;
			this.label = label;
		}

		public GoStruct<?> getTag() {
			return tag;
		}

		public int getIndex() {
			return index;
		}

		public Label getLabel() {
			return label;
		}
	}
}
