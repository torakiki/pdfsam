/*
 * This file is part of the PDF Black project
 * Created on 10 gen 2021
 * Copyright 2021 by Sober Lemur S.a.s di Vacondio Andrea (info@soberlemur.com).
 *
 * You are not permitted to distribute it in any form unless explicit
 * consent is given by Sober Lemur S.a.s di Vacondio Andrea.
 * You are not permitted to modify it.
 *
 * PDF Black is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */
package org.pdfsam.core.context;

import java.util.function.Supplier;

/**
 * Configurable Integer value property
 *
 * @author Andrea Vacondio
 */
public enum IntegerPersistentProperty implements PersistentProperty<Integer> {
    LOGVIEW_ROWS_NUMBER(() -> 200);

    private final Supplier<Integer> defaultSupplier;

    IntegerPersistentProperty(Supplier<Integer> supplier) {
        this.defaultSupplier = supplier;
    }

    @Override
    public String key() {
        return this.name().toLowerCase();
    }

    @Override
    public Supplier<Integer> defaultSupplier() {
        return defaultSupplier;
    }
}
