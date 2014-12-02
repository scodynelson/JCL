package jcl.compiler.real.environment;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Environment implements LispStruct {

	public static final Environment FREE = new Environment(null, Marker.LAMBDA, 0);
	public static final Environment NULL = new Environment(null, Marker.LAMBDA, 0);

	private final Environment parent;
	private final Marker marker;

	private final List<LoadTimeValue> loadTimeValues = new ArrayList<>();
	private final List<Binding> bindings = new ArrayList<>();
	private final SymbolTable symbolTable = new SymbolTable();
	private final Closure environmentClosure;

	// TODO: load-time-value ???
	public Environment(final Environment parent, final Marker marker, final int closureDepth) {
		this.parent = parent;
		this.marker = marker;
		environmentClosure = new Closure(closureDepth);
	}

	public Environment getParent() {
		return parent;
	}

	public Marker getMarker() {
		return marker;
	}

	public List<LoadTimeValue> getLoadTimeValues() {
		return loadTimeValues;
	}

	public List<Binding> getBindings() {
		return bindings;
	}

	public Optional<Binding> getBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		               .findFirst();
	}

	public void addBinding(final SymbolStruct<?> newVariable, final int position, final LispStruct initForm) {
		final Scope scope = newVariable.isSpecial() ? Scope.DYNAMIC : Scope.LEXICAL;
		final Binding binding = new EnvironmentBinding(newVariable, position, scope, T.INSTANCE, initForm);
		bindings.add(binding);
	}

	public SymbolTable getSymbolTable() {
		return symbolTable;
	}

	public Closure getEnvironmentClosure() {
		return environmentClosure;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
