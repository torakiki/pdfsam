/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 24/giu/2014
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
package org.pdfsam.splitbybookmarks;

import org.pdfsam.support.params.SplitParametersBuilder;
import org.sejda.model.parameter.SplitByOutlineLevelParameters;

/**
 * Builder for a SplitByGoToActionLevelParameters
 * 
 * @author Andrea Vacondio
 *
 */
class SplitByOutlineLevelParametersBuilder extends SplitParametersBuilder<SplitByOutlineLevelParameters> {

    private int level = 0;
    private String regexp;

    void level(int level) {
        this.level = level;
    }

    void regexp(String regexp) {
        this.regexp = regexp;
    }

    @Override
    public SplitByOutlineLevelParameters build() {
        SplitByOutlineLevelParameters params = new SplitByOutlineLevelParameters(level);
        params.setCompress(isCompress());
        params.setExistingOutputPolicy(existingOutput());
        params.setVersion(getVersion());
        params.setMatchingTitleRegEx(regexp);
        params.setOutput(getOutput());
        params.setOutputPrefix(getPrefix());
        params.addSource(getSource());
        params.setOptimizationPolicy(getOptimizationPolicy());
        params.discardOutline(isDiscardBookmarks());
        return params;
    }
}
