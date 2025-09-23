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

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Alessandro Parisi
 */
public class WorkspaceTest {

    @Test
    void testEquals() {
        Map<String, String> fooData = new LinkedHashMap<>();
        fooData.put("a", "1");
        fooData.put("b", "true");
        Map<String, String> barData = new LinkedHashMap<>();
        barData.put("a", "0");
        barData.put("b", "false");
        Map<String, Map<String, String>> data = new LinkedHashMap<>();
        data.put("foo", fooData);
        data.put("bar", barData);

        Map<String, Map<String, String>> unorderedCopy = new HashMap<>();
        unorderedCopy.put("foo", new HashMap<>(fooData));
        unorderedCopy.put("bar", new HashMap<>(barData));

        assertTrue(new Workspace(data).equals(unorderedCopy));
    }

    @Test
    void testNotEqualsSize() {
        Map<String, String> fooData = new LinkedHashMap<>();
        fooData.put("a", "1");
        fooData.put("b", "true");
        Map<String, String> barData = new LinkedHashMap<>();
        barData.put("a", "0");
        barData.put("b", "false");
        Map<String, Map<String, String>> data = new LinkedHashMap<>();
        data.put("foo", fooData);
        data.put("bar", barData);
        Workspace workspace = new Workspace(Map.copyOf(data));

        Map<String, String> bazData = new LinkedHashMap<>();
        bazData.put("x", "3.14");
        bazData.put("y", "center");
        data.put("baz", bazData);

        assertFalse(workspace.equals(data));
    }

    @Test
    void testNotEqualsValues() {
        Map<String, String> fooData = new LinkedHashMap<>();
        fooData.put("a", "1");
        fooData.put("b", "true");
        Map<String, String> barData = new LinkedHashMap<>();
        barData.put("a", "0");
        barData.put("b", "false");
        Map<String, Map<String, String>> data = new LinkedHashMap<>();
        data.put("foo", Map.copyOf(fooData));
        data.put("bar", Map.copyOf(barData));
        Workspace workspace = new Workspace(Map.copyOf(data));

        barData.put("b", "true");
        data.put("bar", barData);

        assertFalse(workspace.equals(data));
    }

    @Test
    void testNotEqualsNewValues() {
        Map<String, String> fooData = new LinkedHashMap<>();
        fooData.put("a", "1");
        fooData.put("b", "true");
        Map<String, String> barData = new LinkedHashMap<>();
        barData.put("a", "0");
        barData.put("b", "false");
        Map<String, Map<String, String>> data = new LinkedHashMap<>();
        data.put("foo", Map.copyOf(fooData));
        data.put("bar", Map.copyOf(barData));
        Workspace workspace = new Workspace(Map.copyOf(data));

        barData.put("x", "3.14");
        data.put("bar", barData);

        assertFalse(workspace.equals(data));
    }
}
