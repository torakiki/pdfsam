/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.test;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.util.Arrays;
import java.util.Set;

import org.junit.rules.ExternalResource;
import org.sejda.commons.collection.NullSafeSet;

/**
 * Rule that makes sure that event studio listeners are cleared after each test
 * 
 * @author Andrea Vacondio
 *
 */
public class ClearEventStudioRule extends ExternalResource {

    private Set<String> stations = new NullSafeSet<>();

    public ClearEventStudioRule(String... stations) {
        Arrays.stream(stations).forEach(this.stations::add);
    }

    private void clearAll() {
        eventStudio().clear();
        stations.forEach(s -> eventStudio().clear(s));
    }

    @Override
    protected void after() {
        clearAll();
    }
}
