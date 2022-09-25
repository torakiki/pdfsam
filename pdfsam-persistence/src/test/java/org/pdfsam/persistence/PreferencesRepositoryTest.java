/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13 sep 2022
 * Copyright 2022 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.persistence;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
public class PreferencesRepositoryTest {

    private static PreferencesRepository victim;

    @BeforeAll
    public static void setUp() {
        victim = new PreferencesRepository("/test/org/pdfsam/string");
    }

    @AfterAll
    public static void tearDown() throws PersistenceException {
        victim.clean();
    }

    @Test
    public void blankPath() {
        assertThrows(IllegalArgumentException.class, () -> new PreferencesRepository("  "));
    }

    @Test
    public void nullPath() {
        assertThrows(IllegalArgumentException.class, () -> new PreferencesRepository(null));
    }

    @Test
    @DisplayName("Saving the NUL character throws an exception")
    public void negativesaveString() {
        assertThrows(PersistenceException.class, () -> victim.saveString("key3", Character.toString('\0')));
    }

    @Test
    public void saveBlankKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.saveString("  ", "value"));
    }

    @Test
    public void saveNullKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.saveString(null, "value"));
    }

    @Test
    @DisplayName("Positive scenario for Save")
    public void positivesaveString() throws PersistenceException {
        victim.saveString("key1", "value");
        assertEquals("value", victim.getString("key1", (String) null));
    }

    @Test
    @DisplayName("Save a null value to replace an existing one")
    public void saveNullExisting() throws PersistenceException {
        victim.saveString("key2", "value");
        assertEquals("value", victim.getString("key2", (String) null));
        victim.saveString("key2", null);
        assertNull(victim.getString("key2", (String) null));
    }

    @Test
    @DisplayName("Save a null value to a non existing key")
    public void saveNullNonExisting() throws PersistenceException {
        assertNull(victim.getString("key3", (String) null));
        victim.saveString("key3", null);
        assertNull(victim.getString("key3", (String) null));
    }

    @Test
    @DisplayName("Positive scenario for Get")
    public void positiveGet() throws PersistenceException {
        victim.saveString("key4", "value");
        assertEquals("value", victim.getString("key4", (String) null));
    }

    @Test
    @DisplayName("Default value for Get")
    public void getDefault() throws PersistenceException {
        victim.delete("key5");
        assertEquals("Chuck", victim.getString("key5", "Chuck"));
    }

    @Test
    @DisplayName("Non exsting key returns empty")
    public void defaultGet() throws PersistenceException {
        victim.delete("key6");
        assertNull(victim.getString("key6", (String) null));
    }

    @Test
    public void getBlankKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.getString("  ", (String) null));
    }

    @Test
    public void getNullKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.getString(null, "value"));
    }

    @Test
    @DisplayName("Getting the NUL character throws an exception")
    public void negativeGet() {
        assertThrows(PersistenceException.class,
                () -> victim.getString("key7" + Character.toString('\0'), (String) null));
    }

    @Test
    public void deleteBlankKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.delete("  "));
    }

    @Test
    public void deleteNullKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.delete(null));
    }

    @Test
    @DisplayName("Deleting a key containing the NUL character throws an exception")
    public void negativeDelete() {
        assertThrows(PersistenceException.class, () -> victim.delete("key8" + Character.toString('\0')));
    }

    @Test
    @DisplayName("Positive scenario for Delete")
    public void positiveDelete() throws PersistenceException {
        victim.saveString("key9", "value");
        assertEquals("value", victim.getString("key9", (String) null));
        victim.delete("key9");
        assertNull(victim.getString("key9", () -> null));
    }

    @Test
    @DisplayName("Positive scenario for Clean")
    public void positiveClean() throws PersistenceException {
        victim.saveString("key10", "value");
        victim.saveString("key11", "value");
        assertEquals("value", victim.getString("key10", (String) null));
        assertEquals("value", victim.getString("key11", () -> null));
        victim.clean();
        assertNull(victim.getString("key10", (String) null));
        assertNull(victim.getString("key11", () -> null));
    }

    @Test
    @DisplayName("Positive scenario for GetInt")
    public void positiveGetInt() throws PersistenceException {
        victim.saveInt("key13", 25);
        assertEquals(25, victim.getInt("key13", -1));
    }

    @Test
    @DisplayName("Default value for GetInt")
    public void defaultGetInt() throws PersistenceException {
        victim.clean();
        assertEquals(-1, victim.getInt("key14", -1));
    }

    @Test
    @DisplayName("Positive scenario for GetLong")
    public void positiveGetLong() throws PersistenceException {
        victim.saveLong("keyL4", 25);
        assertEquals(25, victim.getLong("keyL4", -1));
    }

    @Test
    @DisplayName("Default value for GetLong")
    public void defaultGetLong() throws PersistenceException {
        victim.clean();
        assertEquals(-1, victim.getLong("keyL2", -1));
    }

    @Test
    @DisplayName("Positive scenario for GetBoolean")
    public void positiveGetBoolean() throws PersistenceException {
        victim.saveBoolean("keyb1", true);
        assertTrue(victim.getBoolean("keyb1", false));
    }

    @Test
    @DisplayName("Default value for GetBoolean")
    public void defaultGetBoolean() throws PersistenceException {
        victim.clean();
        assertFalse(victim.getBoolean("keyb2", () -> false));
    }

    @Test
    public void keys() throws PersistenceException {
        victim.clean();
        victim.saveString("key14", "value");
        victim.saveString("key15", "value");
        var keys = victim.keys();
        assertThat(keys).hasSize(2);
        assertThat(keys).containsOnly("key14", "key15");
    }
}
