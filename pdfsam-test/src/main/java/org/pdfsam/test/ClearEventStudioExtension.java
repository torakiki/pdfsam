package org.pdfsam.test;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/09/22
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

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
public class ClearEventStudioExtension implements AfterEachCallback {

    private final List<String> stations;

    public ClearEventStudioExtension(String... stations) {
        this.stations = Arrays.stream(stations).filter(StringUtils::isNotBlank).collect(Collectors.toList());
    }

    public ClearEventStudioExtension() {
        this.stations = Collections.emptyList();
    }

    @Override
    public void afterEach(ExtensionContext context) {
        eventStudio().clear();
        stations.forEach(eventStudio()::clear);
    }
}
