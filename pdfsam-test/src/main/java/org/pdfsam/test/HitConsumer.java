/*
 * This file is part of the PDF Split And Merge source code
 * Created on 31/lug/2014
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
package org.pdfsam.test;

import java.util.function.Consumer;

/**
 * Test listener to verify the consumer has been called
 * 
 * @author Andrea Vacondio
 *
 */
public class HitConsumer<T> implements Consumer<T> {

    private boolean hit = false;

    public boolean isHit() {
        return hit;
    }

    @Override
    public void accept(T t) {
        this.hit = true;

    }

}
