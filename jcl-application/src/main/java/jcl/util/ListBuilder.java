package jcl.util;

import java.util.List;

import org.apache.commons.lang3.builder.Builder;

public interface ListBuilder<T> extends Builder<T> {

	List<T> buildList();
}
