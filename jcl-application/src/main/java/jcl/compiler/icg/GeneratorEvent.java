package jcl.compiler.icg;

import jcl.lang.LispStruct;
import org.springframework.context.ApplicationEvent;
import org.springframework.core.ResolvableType;
import org.springframework.core.ResolvableTypeProvider;

public class GeneratorEvent<T extends LispStruct> extends ApplicationEvent implements ResolvableTypeProvider {

	private static final long serialVersionUID = -5938308196500763237L;

	private final GeneratorState generatorState;

	/**
	 * Create a new ApplicationEvent.
	 *
	 * @param source
	 * 		the object on which the event initially occurred (never {@code null})
	 * @param generatorState
	 */
	private GeneratorEvent(final T source, final GeneratorState generatorState) {
		super(source);
		this.generatorState = generatorState;
	}

	public GeneratorState getGeneratorState() {
		return generatorState;
	}

	@Override
	@SuppressWarnings("unchecked")
	public T getSource() {
		return (T) source;
	}

	@Override
	public ResolvableType getResolvableType() {
		return ResolvableType.forClassWithGenerics(getClass(), ResolvableType.forInstance(getSource()));
	}

	public static <T extends LispStruct> GeneratorEvent<T> of(final T source, final GeneratorState generatorState) {
		return new GeneratorEvent<>(source, generatorState);
	}
}
