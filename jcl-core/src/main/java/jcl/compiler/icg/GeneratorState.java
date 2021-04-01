package jcl.compiler.icg;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.Set;

import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.lang.SymbolStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.objectweb.asm.Label;

@Getter
public class GeneratorState {

	private final Deque<JavaClassBuilder> finalClassBuilderDeque = new ArrayDeque<>();
	private final Deque<JavaClassBuilder> classBuilderDeque = new ArrayDeque<>();
	private final Deque<JavaMethodBuilder> methodBuilderDeque = new ArrayDeque<>();
	private final Set<SymbolStruct> lexicalSymbols = new HashSet<>();
	private final Set<SymbolStruct> dynamicSymbols = new HashSet<>();
	private final Deque<Set<TagbodyLabel>> tagbodyLabelDeque = new ArrayDeque<>();

	private int tagCounter;

	public GeneratorState() {
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

	public JavaEnvironmentMethodBuilder getCurrentEnvironmentMethodBuilder() {
		// TODO: we can probably do this better...
		final JavaMethodBuilder currentMethodBuilder = getCurrentMethodBuilder();
		if (currentMethodBuilder instanceof JavaEnvironmentMethodBuilder) {
			return (JavaEnvironmentMethodBuilder) currentMethodBuilder;
		}
		return null;
	}

	@Getter
	@AllArgsConstructor
	public static class TagbodyLabel {
		private final GoStruct<?> tag;
		private final int index;
		private final Label label;
	}
}
