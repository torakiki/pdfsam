/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/ago/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.service.tool;

import org.junit.jupiter.api.Test;

import static java.time.Duration.ofMillis;
import static org.awaitility.Awaitility.await;
import static org.hamcrest.Matchers.greaterThan;

/**
 * @author Andrea Vacondio
 */
public class ToolUsageTest {

    @Test
    public void lastSeenIsUpdated() {
        var victim = new ToolUsage("ChuckNorris");
        victim.inc();
        long lastSeen = victim.getLastSeen();
        await().atLeast(ofMillis(100)).until(() -> victim.inc().getLastSeen(), greaterThan(lastSeen));

    }
}
