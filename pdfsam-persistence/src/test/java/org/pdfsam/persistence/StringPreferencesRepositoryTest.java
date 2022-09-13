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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class StringPreferencesRepositoryTest {

    private static StringPreferencesRepository victim;

    @BeforeAll
    public static void setUp() {
        victim = new StringPreferencesRepository("/test/org/pdfsam/settings");
    }

    @AfterAll
    public static void tearDown() throws PersistenceException {
        victim.clean();
    }

    @Test
    public void blankPath() {
        assertThrows(IllegalArgumentException.class, () -> new StringPreferencesRepository("  "));
    }

    @Test
    public void nullPath() {
        assertThrows(IllegalArgumentException.class, () -> new StringPreferencesRepository(null));
    }

    @Test
    @DisplayName("Saving the NUL character throws an exception")
    public void negativeSave() {
        assertThrows(PersistenceException.class, () -> victim.save("key3", Character.toString('\0')));
    }

    @Test
    public void saveBlankKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.save("  ", "value"));
    }

    @Test
    public void saveNullKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.save(null, "value"));
    }

    @Test
    @DisplayName("Positive scenario for Save")
    public void positiveSave() throws PersistenceException {
        victim.save("key1", "value");
        assertEquals("value", victim.get("key1")
                                    .get());
    }

    @Test
    @DisplayName("Save a null value to replace an existing one")
    public void saveNullExisting() throws PersistenceException {
        victim.save("key2", "value");
        assertEquals("value", victim.get("key2")
                                    .get());
        victim.save("key2", null);
        assertTrue(victim.get("key2")
                         .isEmpty());
    }

    @Test
    @DisplayName("Save a null value to a non existing key")
    public void saveNullNonExisting() throws PersistenceException {
        assertTrue(victim.get("key3")
                         .isEmpty());
        victim.save("key3", null);
        assertTrue(victim.get("key3")
                         .isEmpty());
    }

    @Test
    @DisplayName("Positive scenario for Get")
    public void positiveGet() throws PersistenceException {
        victim.save("key4", "value");
        assertEquals("value", victim.get("key4")
                                    .get());
    }

    @Test
    @DisplayName("Non exsting key returns empty")
    public void defaultGet() throws PersistenceException {
        victim.delete("key5");
        assertTrue(victim.get("key5")
                         .isEmpty());
    }

    @Test
    public void getBlankKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.get("  "));
    }

    @Test
    public void getNullKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.get(null));
    }

    @Test
    @DisplayName("Getting the NUL character throws an exception")
    public void negativeGet() {
        assertThrows(PersistenceException.class, () -> victim.get("key6" + Character.toString('\0')));
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
        assertThrows(PersistenceException.class, () -> victim.delete("key7" + Character.toString('\0')));
    }

    @Test
    @DisplayName("Positive scenario for Delete")
    public void positiveDelete() throws PersistenceException {
        victim.save("key8", "value");
        assertEquals("value", victim.get("key8")
                                    .get());
        victim.delete("key8");
        assertTrue(victim.get("key8")
                         .isEmpty());
    }

    @Test
    @DisplayName("Positive scenario for Clean")
    public void positiveClean() throws PersistenceException {
        victim.save("key8", "value");
        victim.save("key9", "value");
        assertEquals("value", victim.get("key8")
                                    .get());
        assertEquals("value", victim.get("key9")
                                    .get());
        victim.clean();
        assertTrue(victim.get("key8")
                         .isEmpty());
        assertTrue(victim.get("key9")
                         .isEmpty());
    }

}
