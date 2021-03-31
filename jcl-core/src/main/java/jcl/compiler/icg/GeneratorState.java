package jcl.compiler.icg;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Set;

import jcl.compiler.environment.Environment;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.objectweb.asm.Label;

@Getter
public class GeneratorState {

	private final Deque<JavaClassBuilder> finalClassBuilderDeque = new ArrayDeque<>();
	private final Deque<JavaClassBuilder> classBuilderDeque = new ArrayDeque<>();
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

	public JavaClassBuilder getCurrentClassBuilder() {
		if (classBuilderDeque.isEmpty()) {
			return null;
		}
		return classBuilderDeque.peek();
	}

	public JavaMethodBuilder getCurrentMethodBuilder() {
		if (methodBuilderDeque.isEmpty()) {
			return null;
		}
		return methodBuilderDeque.peek();
	}

	public Environment getCurrentEnvironment() {
		if (environmentDeque.isEmpty()) {
			return null;
		}
		return environmentDeque.peek();
	}

	@Getter
	@AllArgsConstructor
	public static class TagbodyLabel {
		private final GoStruct<?> tag;
		private final int index;
		private final Label label;
	}
}
