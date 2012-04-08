/*
 * Created on 08/apr/2012
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.support;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Fixed size {@link Map} implementation removing the least recently used element when {@link Map#put(Object, Object)} is called.
 * 
 * @author Andrea Vacondio
 * @param <K>
 *            key of the map
 * @param <V>
 *            value
 */
public class LRUMap<K, V> extends LinkedHashMap<K, V> {

    private int maxCapacity;

    public LRUMap(int maxCapacity) {
        super(maxCapacity, 0.75f, true);
        this.maxCapacity = maxCapacity;
    }

    @Override
    protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
        return size() > this.maxCapacity;
    }
}
