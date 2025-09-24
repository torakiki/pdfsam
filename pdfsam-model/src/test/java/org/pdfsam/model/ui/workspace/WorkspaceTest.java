/*
 * This file is part of the PDF Split And Merge source code
 * Created on 23/09/2025
 * Copyright 2025 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.model.ui.workspace;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Alessandro Parisi
 */
public class WorkspaceTest {

    @Test
    void containsAllIgnoreEmpty() {
        Map<String, String> fooData = Map.of("a", "1", "b", "true");
        Map<String, String> barData = Map.of("a", "0", "b", "false");
        Map<String, Map<String, String>> data = Map.of("foo", fooData, "bar", barData);
        Map<String, Map<String, String>> unorderedCopy = Map.of("bar", new HashMap<>(barData), "foo",
                new HashMap<>(fooData));

        assertTrue(new Workspace(data, null).containsAllIgnoreEmpty(unorderedCopy));
    }

    @Test
    void testNotEqualsSize() {
        Map<String, String> fooData = Map.of("a", "1", "b", "true");
        Map<String, String> barData = Map.of("a", "0", "b", "false");
        Map<String, Map<String, String>> data = Map.of("foo", fooData, "bar", barData);

        Map<String, Map<String, String>> bazData = new HashMap<>(data);
        bazData.put("baz", Map.of("x", "3.14", "y", "center"));

        assertFalse(new Workspace(data, null).containsAllIgnoreEmpty(bazData));
    }

    @Test
    void testNotEqualsValues() {
        Map<String, String> fooData = Map.of("a", "1", "b", "true");
        Map<String, String> barData = Map.of("a", "0", "b", "false");
        Map<String, Map<String, String>> data = Map.of("foo", fooData, "bar", barData);

        Map<String, String> barDataDif = Map.of("a", "0", "b", "true");
        Map<String, Map<String, String>> diffData = Map.of("foo", fooData, "bar", barDataDif);

        assertFalse(new Workspace(data, null).containsAllIgnoreEmpty(diffData));
    }

    @Test
    void testNotEqualsNewValues() {
        Map<String, String> fooData = Map.of("a", "1", "b", "true");
        Map<String, String> barData = Map.of("a", "0", "b", "false");
        Map<String, Map<String, String>> data = Map.of("foo", fooData, "bar", barData);

        Map<String, String> barDataDif = Map.of("a", "0", "b", "false", "x", "3.14");
        Map<String, Map<String, String>> diffData = Map.of("foo", fooData, "bar", barDataDif);

        assertFalse(new Workspace(data, null).containsAllIgnoreEmpty(diffData));
    }

    @Test
    void containsAllIgnoresEmptyValues() {
        Map<String, String> fooData = Map.of("a", "1", "b", "true");
        Map<String, String> barData = Map.of("a", "0", "b", "false");
        Map<String, String> barDataWithEmpty = Map.of("a", "0", "b", "false", "c", "");
        Map<String, Map<String, String>> data = Map.of("foo", fooData, "bar", barData);
        Map<String, Map<String, String>> data1 = Map.of("bar", barDataWithEmpty, "foo", fooData);

        assertTrue(new Workspace(data, null).containsAllIgnoreEmpty(data1));
    }

    @Test
    void containsAllIgnoresNullValues() {
        Map<String, String> fooData = Map.of("a", "1", "b", "true");
        Map<String, String> barData = Map.of("a", "0", "b", "false");
        Map<String, String> barDataWithNull = new HashMap<>();
        barDataWithNull.put("a", "0");
        barDataWithNull.put("b", "false");
        barDataWithNull.put("c", null);
        Map<String, Map<String, String>> data = Map.of("foo", fooData, "bar", barData);
        Map<String, Map<String, String>> data1 = Map.of("bar", barDataWithNull, "foo", fooData);

        assertTrue(new Workspace(data, null).containsAllIgnoreEmpty(data1));
    }

    @Test
    void merge() {
        var data = new HashMap<String, Map<String, String>>();
        data.put("foo", Map.of("a", "1", "b", "true"));
        var victim = new Workspace(data, null);

        victim.merge(Map.of("bar", Map.of("a", "0", "b", "false")));
        Assertions.assertThat((victim.data())).containsOnlyKeys("foo", "bar");
    }

    @Test
    @DisplayName("Merge with null doesn't blow up")
    void mergeWithNull() {
        var victim = new Workspace(Map.of("foo", Map.of("a", "1", "b", "true")), null);
        victim.merge(null);
    }
}
