package jcl.structs.symbols.variables;

import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.packages.PackageStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class ProperListVariable extends Variable<ListStruct> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProperListVariable.class);

	ProperListVariable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage, NullStruct.INSTANCE);
	}

	@Override
	public void setValue(final ListStruct value) {

		if (value.isProper()) {
			this.value = value;
		} else {
			LOGGER.warn("Error: {} had illegal value {}. Reset to NIL", name, value);

			this.value = NullStruct.INSTANCE;
		}
	}
}
