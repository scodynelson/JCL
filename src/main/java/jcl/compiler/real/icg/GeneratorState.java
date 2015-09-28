package jcl.compiler.real.icg;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Set;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import org.objectweb.asm.Label;

public class GeneratorState {

	private final Deque<JavaClassBuilder> finalClassBuilderDeque = new ArrayDeque<>();

	private final Deque<JavaClassBuilder> classBuilderDeque = new ArrayDeque<>();

	private final Deque<JCLClassWriter> classWriterDeque = new ArrayDeque<>();

	private JCLClassWriter currentCw;

	private final Deque<JavaMethodBuilder> methodBuilderDeque = new ArrayDeque<>();

	private final Deque<Environment> environmentDeque = new ArrayDeque<>();

	private final Deque<Set<TagbodyLabel>> tagbodyLabelDeque = new ArrayDeque<>();

	private int tagCounter;

	public GeneratorState() {
		environmentDeque.push(Environment.NULL);
		tagCounter = 0;
	}

	public int getNextTagbodyTagIndex() {
		return tagCounter++;
	}

	public Deque<JavaClassBuilder> getFinalClassBuilderDeque() {
		return finalClassBuilderDeque;
	}

	public Deque<JavaClassBuilder> getClassBuilderDeque() {
		return classBuilderDeque;
	}

	public JavaClassBuilder getCurrentClassBuilder() {
		if (classBuilderDeque.isEmpty()) {
			return null;
		}
		return classBuilderDeque.peek();
	}

	public Deque<JCLClassWriter> getClassWriterDeque() {
		return classWriterDeque;
	}

	public JCLClassWriter getCurrentCw() {
		return currentCw;
	}

	public void setCurrentCw(final JCLClassWriter currentCw) {
		this.currentCw = currentCw;
	}

	public Deque<JavaMethodBuilder> getMethodBuilderDeque() {
		return methodBuilderDeque;
	}

	public JavaMethodBuilder getCurrentMethodBuilder() {
		if (methodBuilderDeque.isEmpty()) {
			return null;
		}
		return methodBuilderDeque.peek();
	}

	public Deque<Environment> getEnvironmentDeque() {
		return environmentDeque;
	}

	public Environment getCurrentEnvironment() {
		if (environmentDeque.isEmpty()) {
			return null;
		}
		return environmentDeque.peek();
	}

	public Deque<Set<TagbodyLabel>> getTagbodyLabelDeque() {
		return tagbodyLabelDeque;
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
