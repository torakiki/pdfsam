/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/ott/2014
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
package org.pdfsam.ui;

import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

/**
 * Holds data regarding the status of the main stage
 * 
 * @author Andrea Vacondio
 *
 */
public class StageStatus {

    public static final StageStatus NULL = new StageStatus(0, 0, 0, 0) {
        @Override
        public void setX(double x) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void setY(double y) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void setWidth(double width) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void setHeight(double height) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void setMode(StageMode mode) {
            throw new UnsupportedOperationException();
        }
    };

    private double x;
    private double y;
    private double width;
    private double height;
    private StageMode mode;

    StageStatus() {
        // jackson
    }

    public StageStatus(double x, double y, double width, double height) {
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
        this.mode = StageMode.DEFAULT;
    }

    public double getX() {
        return x;
    }

    public void setX(double x) {
        this.x = x;
    }

    public double getY() {
        return y;
    }

    public void setY(double y) {
        this.y = y;
    }

    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height;
    }

    public void setMode(StageMode mode) {
        requireNotNullArg(mode, "Stage mode cannot be null");
        this.mode = mode;
    }

    public StageMode getMode() {
        return mode;
    }

    @Override
    public String toString() {
        return ReflectionToStringBuilder.toString(this);
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(x).append(y).append(width).append(height).append(mode).toHashCode();
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof StageStatus)) {
            return false;
        }
        StageStatus item = (StageStatus) other;
        return new EqualsBuilder().append(x, item.getX()).append(y, item.getY()).append(width, item.getWidth())
                .append(height, item.getHeight()).append(mode, item.getMode()).isEquals();
    }
}
