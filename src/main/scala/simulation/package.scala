package object simulation {
  type AgentId = Long
  type ObjectId = Long

  /**
    * Unique id for the environment to send message to it
    */
  val ENVIRONMENT_ID = 0

  object Generator {
    var lastAgentId: AgentId = 0
    var lastObjectId: ObjectId = 0

    /**
      * Generates a new id for an agent and returns it
      * @return id for an agent
      */
    def getNextAgentId: AgentId = {
      lastAgentId = lastAgentId + 1
      lastAgentId
    }

    /**
      * Generates a new id for an object and returns it
      * @return id for an object, e.g., product or bank account
      */
    def getNextObjectId: ObjectId = {
      lastObjectId = lastObjectId + 1
      lastObjectId
    }

  }

}
