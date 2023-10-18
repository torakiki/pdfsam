/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12 feb 2021
 * Copyright 2021 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.core.context;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.SetSystemProperty;
import org.pdfsam.core.ConfigurableSystemProperty;
import org.pdfsam.persistence.PersistenceException;
import org.pdfsam.persistence.PreferencesRepository;

import java.util.function.Supplier;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 */
public class ApplicationPersistentSettingsTest {

    private ApplicationPersistentSettings victim;
    private PreferencesRepository repo;

    @BeforeEach
    public void setUp() {
        repo = mock(PreferencesRepository.class);
        victim = new ApplicationPersistentSettings(repo);
    }

    @Test
    @DisplayName("String value is set and notified")
    public void setString() throws PersistenceException {
        var testListener = victim.settingsChanges(StringPersistentProperty.LOCALE).test();
        victim.set(StringPersistentProperty.LOCALE, "it");
        verify(repo).saveString(StringPersistentProperty.LOCALE.key(), "it");
        testListener.assertValue(of("it"));
    }

    @Test
    @DisplayName("Null string value is set and notified")
    public void setNullString() throws PersistenceException {
        var testListener = victim.settingsChanges(StringPersistentProperty.LOCALE).test();
        victim.set(StringPersistentProperty.LOCALE, null);
        verify(repo).saveString(StringPersistentProperty.LOCALE.key(), null);
        testListener.assertValue(empty());
    }

    @Test
    @DisplayName("Blank string value is set and notified")
    public void setBlankString() throws PersistenceException {
        var testListener = victim.settingsChanges(StringPersistentProperty.LOCALE).test();
        victim.set(StringPersistentProperty.LOCALE, "   ");
        verify(repo).saveString(StringPersistentProperty.LOCALE.key(), "   ");
        testListener.assertValue(of("   "));
    }

    @Test
    @DisplayName("Failing repo string value is not notified")
    public void negativeSetString() throws PersistenceException {
        doThrow(PersistenceException.class).when(repo).saveString(StringPersistentProperty.LOCALE.key(), "it");
        var testListener = victim.settingsChanges(StringPersistentProperty.LOCALE).test();
        victim.set(StringPersistentProperty.LOCALE, "it");
        verify(repo).saveString(StringPersistentProperty.LOCALE.key(), "it");
        testListener.assertNoValues();
    }

    @Test
    @DisplayName("String value null prop")
    public void nullSetString() {
        assertThrows(IllegalArgumentException.class, () -> victim.set(null, "it"));
    }

    @Test
    @DisplayName("Integer value is set and notified")
    public void setInteger() throws PersistenceException {
        var testListener = victim.settingsChanges(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER).test();
        victim.set(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER, 14);
        verify(repo).saveInt(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER.key(), 14);
        testListener.assertValue(of(14));
    }

    @Test
    @DisplayName("Failing repo integer value is not notified")
    public void negativeSetInteger() throws PersistenceException {
        doThrow(PersistenceException.class).when(repo).saveInt(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER.key(), 14);
        var testListener = victim.settingsChanges(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER).test();
        victim.set(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER, 14);
        verify(repo).saveInt(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER.key(), 14);
        testListener.assertNoValues();
    }

    @Test
    @DisplayName("Integer value null prop")
    public void nullSetInteger() {
        assertThrows(IllegalArgumentException.class, () -> victim.set(null, 14));
    }

    @Test
    @DisplayName("Boolean value is set and notified")
    public void setBoolean() throws PersistenceException {
        var testListener = victim.settingsChanges(BooleanPersistentProperty.OVERWRITE_OUTPUT).test();
        victim.set(BooleanPersistentProperty.OVERWRITE_OUTPUT, true);
        verify(repo).saveBoolean(BooleanPersistentProperty.OVERWRITE_OUTPUT.key(), true);
        testListener.assertValue(of(true));
    }

    @Test
    @DisplayName("Failing repo boolean value is not notified")
    public void negativeSetBoolean() throws PersistenceException {
        doThrow(PersistenceException.class).when(repo)
                .saveBoolean(BooleanPersistentProperty.OVERWRITE_OUTPUT.key(), true);
        var testListener = victim.settingsChanges(BooleanPersistentProperty.OVERWRITE_OUTPUT).test();
        victim.set(BooleanPersistentProperty.OVERWRITE_OUTPUT, true);
        verify(repo).saveBoolean(BooleanPersistentProperty.OVERWRITE_OUTPUT.key(), true);
        testListener.assertNoValues();
    }

    @Test
    @DisplayName("Boolean value null prop")
    public void nullSetBoolean() {
        assertThrows(IllegalArgumentException.class, () -> victim.set(null, true));
    }

    @Test
    @DisplayName("Get string value")
    public void getString() throws PersistenceException {
        when(repo.getString(anyString(), any(Supplier.class))).thenReturn("it");
        assertEquals("it", victim.get(StringPersistentProperty.LOCALE).get());
    }

    @Test
    @DisplayName("Failing repo get string value")
    @SetSystemProperty(key = ConfigurableSystemProperty.LOCALE_PROP, value = "es")
    public void negativeGetString() throws PersistenceException {
        doThrow(PersistenceException.class).when(repo).getString(anyString(), any(Supplier.class));
        assertEquals("es", victim.get(StringPersistentProperty.LOCALE).get());
    }

    @Test
    @DisplayName("Get string returns no value")
    @SetSystemProperty(key = ConfigurableSystemProperty.LOCALE_PROP, value = "es")
    public void emptyGetString() throws PersistenceException {
        var repo = new PreferencesRepository("/test/org/pdfsam/delete");
        var victim = new ApplicationPersistentSettings(repo);
        assertEquals("es", victim.get(StringPersistentProperty.LOCALE).get());
        repo.clean();
    }

    @Test
    public void nullGetString() {
        assertThrows(IllegalArgumentException.class, () -> victim.get((StringPersistentProperty) null));
    }

    @Test
    @DisplayName("Get integer value")
    public void getInteger() throws PersistenceException {
        when(repo.getInt(anyString(), any(Supplier.class))).thenReturn(14);
        assertEquals(14, victim.get(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER));
    }

    @Test
    @DisplayName("Failing repo get integer value")
    public void negativeGetInteger() throws PersistenceException {
        doThrow(PersistenceException.class).when(repo).getInt(anyString(), any(Supplier.class));
        assertEquals(200, victim.get(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER));
    }

    @Test
    public void nullGetInteger() {
        assertThrows(IllegalArgumentException.class, () -> victim.get((IntegerPersistentProperty) null));
    }

    @Test
    @DisplayName("Get boolean value")
    public void getBoolean() throws PersistenceException {
        when(repo.getBoolean(anyString(), any(Supplier.class))).thenReturn(false);
        assertFalse(victim.get(BooleanPersistentProperty.CLEAR_CONFIRMATION));
    }

    @Test
    @DisplayName("Failing repo get boolean value")
    public void negativeGetBoolean() throws PersistenceException {
        when(repo.getBoolean(anyString(), any(Supplier.class))).thenThrow(PersistenceException.class);
        assertTrue(victim.get(BooleanPersistentProperty.CLEAR_CONFIRMATION));
    }

    @Test
    public void nullGetBoolean() {
        assertThrows(IllegalArgumentException.class, () -> victim.get((BooleanPersistentProperty) null));
    }

    @Test
    public void clean() throws PersistenceException {
        victim.clean();
        verify(repo).clean();
    }

    @Test
    public void close() {
        var intSettings = victim.settingsChanges(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER).test();
        var stringSettings = victim.settingsChanges(StringPersistentProperty.LOCALE).test();
        var booleanSettings = victim.settingsChanges(BooleanPersistentProperty.CHECK_FOR_NEWS).test();
        victim.close();
        intSettings.assertComplete();
        stringSettings.assertComplete();
        booleanSettings.assertComplete();
    }

    @Test
    void hasValueFor() {
        when(repo.keys()).thenReturn(new String[] {});
        assertFalse(victim.hasValueFor(StringPersistentProperty.LOCALE));
        when(repo.keys()).thenReturn(new String[] { "theme" });
        assertFalse(victim.hasValueFor(StringPersistentProperty.LOCALE));
        when(repo.keys()).thenReturn(new String[] { "theme", "locale" });
        assertTrue(victim.hasValueFor(StringPersistentProperty.LOCALE));
        when(repo.keys()).thenReturn(new String[] { "locale" });
        assertTrue(victim.hasValueFor(StringPersistentProperty.LOCALE));
    }

    @Test
    void hasValueForNullProperty() {
        assertFalse(victim.hasValueFor(null));
    }
}
