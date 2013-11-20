/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/set/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.event;

import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author Andrea Vacondio
 * 
 */
public class EventSubscriberTemplateTest {

    private String parent = String.newParentInstance("john.doe");
    private String child = String.newChildInstance(parent, "child");
    private String unrelated = String.newParentInstance("chuck.norris");

    @Test
    public void executes() {
        HitCallback callback = new HitCallback();
        EventSubscriberTemplate.ifEvent(new ModuleEvent(parent)).routesTo(child).execute(callback);
        assertTrue(callback.isHit());
    }

    @Test
    public void doesntExecutes() {
        HitCallback callback = new HitCallback();
        EventSubscriberTemplate.ifEvent(new ModuleEvent(parent)).routesTo(unrelated).execute(callback);
        assertFalse(callback.isHit());
    }

    private class HitCallback implements EventSubscriberCallback {

        private boolean hit = false;

        public void exec(ModuleEvent e) {
            hit = true;
        }

        public boolean isHit() {
            return hit;
        }

    }
}
