package jcl.structs.symbols.special;

import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.packages.PackageStruct;
import jcl.structs.symbols.Variable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProperListVariable extends Variable<ListStruct> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProperListVariable.class);

	public ProperListVariable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage, NullStruct.INSTANCE);
	}

	@Override
	public void setValue(final ListStruct value) {

		if (value.isProper()) {
			this.value = value;
		} else {
			LOGGER.warn("Error: {} had illegal value {}.  Reset to NIL", name, value);

			this.value = NullStruct.INSTANCE;
		}
	}
}
