/*
 * This file is part of the PDF Black project
 * Created on 11 feb 2021
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

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Andrea Vacondio
 */
public class IntegerPersistentPropertyTest {
    @Test
    public void defaultValue() {
        assertEquals(200, IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER.defaultSupplier().get());
    }

}
