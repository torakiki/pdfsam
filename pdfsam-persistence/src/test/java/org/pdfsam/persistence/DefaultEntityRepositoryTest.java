/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13 sep 2022
 * Copyright 2022 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * @author Andrea Vacondio
 */
public class DefaultEntityRepositoryTest {
    private static DefaultEntityRepository<Entity> victim;

    @BeforeAll
    public static void setUp() {
        var mapper = JsonMapper.builder().enable(DeserializationFeature.FAIL_ON_NUMBERS_FOR_ENUMS)
                .disable(SerializationFeature.WRITE_DATE_TIMESTAMPS_AS_NANOSECONDS)
                .visibility(PropertyAccessor.FIELD, Visibility.ANY)
                .configure(MapperFeature.CAN_OVERRIDE_ACCESS_MODIFIERS, false)
                .defaultPropertyInclusion(JsonInclude.Value.ALL_ALWAYS)
                .build();
        victim = new DefaultEntityRepository<>("/test/org/pdfsam/entity", mapper, Entity.class);
    }

    @AfterAll
    public static void tearDown() throws PersistenceException {
        victim.clean();
    }

    @Test
    public void blankPath() {
        assertThrows(IllegalArgumentException.class,
                () -> new DefaultEntityRepository<>(" ", mock(ObjectMapper.class), Entity.class));
    }

    @Test
    public void nullPath() {
        assertThrows(IllegalArgumentException.class,
                () -> new DefaultEntityRepository<>(null, mock(ObjectMapper.class), Entity.class));
    }

    @Test
    public void nullMapper() {
        assertThrows(IllegalArgumentException.class,
                () -> new DefaultEntityRepository<>("/node/path", null, Entity.class));
    }

    @Test
    public void nullClass() {
        assertThrows(IllegalArgumentException.class,
                () -> new DefaultEntityRepository<>("/node/path", mock(ObjectMapper.class), null));
    }

    @Test
    @DisplayName("Saving the NUL character throws an exception")
    public void negativeSave() {
        assertThrows(PersistenceException.class, () -> victim.save("key3" + '\0', new Entity("chuck", 3)));
    }

    @Test
    public void saveBlankKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.save("  ", new Entity("chuck", 3)));
    }

    @Test
    public void saveNullKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.save(null, new Entity("chuck", 3)));
    }

    @Test
    @DisplayName("Positive scenario for Save")
    public void positiveSave() throws PersistenceException {
        var segal = new Entity("Steven", 5);
        victim.save("key1", segal);
        assertEquals(segal, victim.get("key1").get());
    }

    @Test
    @DisplayName("Save a null value to replace an existing one")
    public void saveNullExisting() throws PersistenceException {
        var chuck = new Entity("Chuck", 5);
        victim.save("key2", chuck);
        assertEquals(chuck, victim.get("key2").get());
        victim.save("key2", null);
        assertTrue(victim.get("key2").isEmpty());
    }

    @Test
    @DisplayName("Save a null value to a non existing key")
    public void saveNullNonExisting() throws PersistenceException {
        assertTrue(victim.get("key3").isEmpty());
        victim.save("key3", null);
        assertTrue(victim.get("key3").isEmpty());
    }

    @Test
    @DisplayName("Positive scenario for Get")
    public void positiveGet() throws PersistenceException {
        var chuck = new Entity("Chuck", 5);
        victim.save("key4", chuck);
        assertEquals(chuck, victim.get("key4").get());
    }

    @Test
    @DisplayName("Default value for Get")
    public void defaultValueGet() throws PersistenceException {
        var chuck = new Entity("Chuck", 5);
        victim.delete("key4");
        assertEquals(chuck, victim.get("key4", chuck));
    }

    @Test
    @DisplayName("Non existing key returns empty")
    public void defaultGet() throws PersistenceException {
        victim.delete("key5");
        assertTrue(victim.get("key5").isEmpty());
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
        assertThrows(PersistenceException.class, () -> victim.get("key6" + '\0'));
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
        assertThrows(PersistenceException.class, () -> victim.delete("key7" + '\0'));
    }

    @Test
    @DisplayName("Positive scenario for Delete")
    public void positiveDelete() throws PersistenceException {
        var jcvd = new Entity("JC", 2);
        victim.save("key8", jcvd);
        assertEquals(jcvd, victim.get("key8").get());
        victim.delete("key8");
        assertTrue(victim.get("key8").isEmpty());
    }

    @Test
    @DisplayName("Positive scenario for Clean")
    public void positiveClean() throws PersistenceException {
        var jcvd = new Entity("JC", 2);
        var steven = new Entity("Steven", 22);
        victim.save("key8", jcvd);
        victim.save("key9", steven);
        assertEquals(jcvd, victim.get("key8").get());
        assertEquals(steven, victim.get("key9").get());
        victim.clean();
        assertTrue(victim.get("key8").isEmpty());
        assertTrue(victim.get("key9").isEmpty());
    }

    public record Entity(String name, Integer roundkicks) {
    }
}
