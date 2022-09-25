package org.pdfsam.core.context;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.ClearSystemProperty;
import org.junitpioneer.jupiter.SetSystemProperty;
import org.pdfsam.core.ConfigurableSystemProperty;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/09/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
class BooleanPersistentPropertyTest {
    @Test
    @SetSystemProperty.SetSystemProperties({
            @SetSystemProperty(key = ConfigurableSystemProperty.PLAY_SOUNDS_PROP, value = "false"),
            @SetSystemProperty(key = ConfigurableSystemProperty.DONATE_NOTIFICATION_PROP, value = "false"),
            @SetSystemProperty(key = ConfigurableSystemProperty.FETCH_PREMIUM_MODULES_PROP, value = "false"),
            @SetSystemProperty(key = ConfigurableSystemProperty.SMART_OUTPUT_PROP, value = "false"),
            @SetSystemProperty(key = ConfigurableSystemProperty.CHECK_FOR_NEWS_PROP, value = "false"),
            @SetSystemProperty(key = ConfigurableSystemProperty.CHECK_FOR_UPDATES_PROP, value = "false"),
            @SetSystemProperty(key = ConfigurableSystemProperty.OVERWRITE_OUTPUT_PROP, value = "true"),
            @SetSystemProperty(key = ConfigurableSystemProperty.PDF_COMPRESSION_PROP, value = "false") })
    @DisplayName("Default value suppliers from sys props")
    public void defaultValuesFromSysProp() {
        assertFalse(BooleanPersistentProperty.PLAY_SOUNDS.defaultSupplier().get());
        assertFalse(BooleanPersistentProperty.DONATION_NOTIFICATION.defaultSupplier().get());
        assertFalse(BooleanPersistentProperty.PREMIUM_MODULES.defaultSupplier().get());
        assertFalse(BooleanPersistentProperty.SMART_OUTPUT.defaultSupplier().get());
        assertFalse(BooleanPersistentProperty.CHECK_FOR_NEWS.defaultSupplier().get());
        assertFalse(BooleanPersistentProperty.CHECK_UPDATES.defaultSupplier().get());
        assertTrue(BooleanPersistentProperty.OVERWRITE_OUTPUT.defaultSupplier().get());
        assertFalse(BooleanPersistentProperty.PDF_COMPRESSION_ENABLED.defaultSupplier().get());
    }

    @Test
    @ClearSystemProperty.ClearSystemProperties({
            @ClearSystemProperty(key = ConfigurableSystemProperty.PLAY_SOUNDS_PROP),
            @ClearSystemProperty(key = ConfigurableSystemProperty.DONATE_NOTIFICATION_PROP),
            @ClearSystemProperty(key = ConfigurableSystemProperty.FETCH_PREMIUM_MODULES_PROP),
            @ClearSystemProperty(key = ConfigurableSystemProperty.SMART_OUTPUT_PROP),
            @ClearSystemProperty(key = ConfigurableSystemProperty.CHECK_FOR_NEWS_PROP),
            @ClearSystemProperty(key = ConfigurableSystemProperty.CHECK_FOR_UPDATES_PROP),
            @ClearSystemProperty(key = ConfigurableSystemProperty.OVERWRITE_OUTPUT_PROP),
            @ClearSystemProperty(key = ConfigurableSystemProperty.PDF_COMPRESSION_PROP) })

    @DisplayName("Default value supplier when no sys props")
    public void defaultValuesClearedSysProp() {
        assertTrue(BooleanPersistentProperty.PLAY_SOUNDS.defaultSupplier().get());
        assertTrue(BooleanPersistentProperty.DONATION_NOTIFICATION.defaultSupplier().get());
        assertTrue(BooleanPersistentProperty.PREMIUM_MODULES.defaultSupplier().get());
        assertTrue(BooleanPersistentProperty.SMART_OUTPUT.defaultSupplier().get());
        assertTrue(BooleanPersistentProperty.CHECK_FOR_NEWS.defaultSupplier().get());
        assertTrue(BooleanPersistentProperty.CHECK_UPDATES.defaultSupplier().get());
        assertFalse(BooleanPersistentProperty.OVERWRITE_OUTPUT.defaultSupplier().get());
        assertTrue(BooleanPersistentProperty.PDF_COMPRESSION_ENABLED.defaultSupplier().get());
    }

    @Test
    public void defaultValues() {
        assertTrue(BooleanPersistentProperty.CLEAR_CONFIRMATION.defaultSupplier().get());
        assertFalse(BooleanPersistentProperty.SAVE_WORKSPACE_ON_EXIT.defaultSupplier().get());
        assertFalse(BooleanPersistentProperty.SAVE_PWD_IN_WORKSPACE.defaultSupplier().get());
    }
}