package jcl.compiler.icg.generator;

import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.struct.specialoperator.go.GoIntegerStruct;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
final class GoIntegerCodeGenerator extends GoCodeGenerator<GoIntegerStruct> {

	@Override
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<GoIntegerStruct> event) {
		super.onGeneratorEvent(event);
	}
}
