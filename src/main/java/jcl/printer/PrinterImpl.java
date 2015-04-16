/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

import java.util.Map;
import javax.annotation.Resource;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

@Component
public class PrinterImpl implements Printer {

	private static final long serialVersionUID = -3051919400352866531L;

	@Resource
	private Map<Class<?>, LispPrinter<LispStruct>> printerStrategies;

	@Override
	public String print(final LispStruct object) {

		final Class<?> objectClass = object.getClass();

		LispPrinter<LispStruct> printer = printerStrategies.get(objectClass);
		if (printer == null) {
			Class<?> superclass = objectClass.getSuperclass();
			while (superclass != null) {

				final LispPrinter<LispStruct> possiblePrinter = printerStrategies.get(superclass);
				if (possiblePrinter != null) {
					printer = possiblePrinter;
					break;
				}
				superclass = superclass.getSuperclass();
			}
		}

		if (printer == null) {
			final String typeClassName = objectClass.getSimpleName();
			return "#<" + typeClassName + '>';
		}

		return printer.print(object);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(printerStrategies)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final PrinterImpl rhs = (PrinterImpl) obj;
		return new EqualsBuilder().append(printerStrategies, rhs.printerStrategies)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printerStrategies)
		                                                                .toString();
	}
}
